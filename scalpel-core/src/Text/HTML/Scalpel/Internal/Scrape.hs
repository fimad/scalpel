{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape (
    Scraper
,   ScraperT (..)
,   scrape
,   scrapeT
,   attr
,   attrs
,   html
,   htmls
,   innerHTML
,   innerHTMLs
,   text
,   texts
,   chroot
,   chroots
,   matches
,   position
) where

import Text.HTML.Scalpel.Internal.Select
import Text.HTML.Scalpel.Internal.Select.Types

import Control.Applicative
import Control.Monad
import Control.Monad.Except (MonadError)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Reader
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Maybe
import Control.Monad.Writer (MonadWriter)
import Data.Functor.Identity
import Data.Maybe

import qualified Control.Monad.Fail as Fail
import qualified Data.Vector as Vector
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


-- | A 'ScraperT' operates like 'Scraper' but also acts as a monad transformer.
newtype ScraperT str m a = MkScraper (ReaderT (TagSpec str) (MaybeT m) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFix,
              MonadIO, MonadCont, MonadError e, MonadState s, MonadWriter w)

#if MIN_VERSION_base(4,9,0)
deriving instance Monad m => Fail.MonadFail (ScraperT str m)
#else
instance Fail.MonadFail m => Fail.MonadFail (ScraperT str m) where
  fail = lift . Fail.fail
#endif

instance MonadTrans (ScraperT str) where
  lift op = MkScraper . lift . lift $ op

instance MonadReader s m => MonadReader s (ScraperT str m) where
  ask = MkScraper (lift . lift $ ask)
  local f (MkScraper op) = (fmap MkScraper . mapReaderT . local) f op

-- | A value of 'Scraper' @a@ defines a web scraper that is capable of consuming
-- a list of 'TagSoup.Tag's and optionally producing a value of type @a@.
type Scraper str = ScraperT str Identity

scrapeTagSpec :: ScraperT str m a -> TagSpec str -> m (Maybe a)
scrapeTagSpec (MkScraper r) = runMaybeT . runReaderT r

-- | The 'scrapeT' function executes a 'ScraperT' on a list of 'TagSoup.Tag's
-- and produces an optional value. Since 'ScraperT' is a monad transformer, the
-- result is monadic.
scrapeT :: (TagSoup.StringLike str)
        => ScraperT str m a -> [TagSoup.Tag str] -> m (Maybe a)
scrapeT s = scrapeTagSpec s . tagsToSpec . TagSoup.canonicalizeTags

-- | The 'scrape' function executes a 'Scraper' on a list of 'TagSoup.Tag's and
-- produces an optional value.
scrape :: (TagSoup.StringLike str)
       => Scraper str a -> [TagSoup.Tag str] -> Maybe a
scrape = fmap runIdentity . scrapeT

-- | The 'chroot' function takes a selector and an inner scraper and executes
-- the inner scraper as if it were scraping a document that consists solely of
-- the tags corresponding to the selector.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'chroots'.
chroot :: (TagSoup.StringLike str, Monad m)
       => Selector -> ScraperT str m a -> ScraperT str m a
chroot selector inner = do
    maybeResult <- listToMaybe <$> chroots selector inner
    guard (isJust maybeResult)
    return $ fromJust maybeResult

-- | The 'chroots' function takes a selector and an inner scraper and executes
-- the inner scraper as if it were scraping a document that consists solely of
-- the tags corresponding to the selector. The inner scraper is executed for
-- each set of tags (possibly nested) matching the given selector.
--
-- > s = "<div><div>A</div></div>"
-- > scrapeStringLike s (chroots "div" (pure 0)) == Just [0, 0]
chroots :: (TagSoup.StringLike str, Monad m)
        => Selector -> ScraperT str m a -> ScraperT str m [a]
chroots selector (MkScraper (ReaderT inner)) =
        MkScraper $ ReaderT $ \tags -> MaybeT $ do
          mvalues <- forM (select selector tags) (runMaybeT . inner)
          return $ Just $ catMaybes mvalues

-- | The 'matches' function takes a selector and returns `()` if the selector
-- matches any node in the DOM.
matches :: (TagSoup.StringLike str, Monad m) => Selector -> ScraperT str m ()
matches s = MkScraper $ (guard . not . null) =<< reader (select s)

-- | The 'text' function takes a selector and returns the inner text from the
-- set of tags described by the given selector.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'texts'.
text :: (TagSoup.StringLike str, Monad m) => Selector -> ScraperT str m str
text s = MkScraper $ withHead tagsToText =<< reader (select s)

-- | The 'texts' function takes a selector and returns the inner text from every
-- set of tags (possibly nested) matching the given selector.
--
-- > s = "<div>Hello <div>World</div></div>"
-- > scrapeStringLike s (texts "div") == Just ["Hello World", "World"]
texts :: (TagSoup.StringLike str, Monad m)
      => Selector -> ScraperT str m [str]
texts s = MkScraper $ withAll tagsToText =<< reader (select s)

-- | The 'html' function takes a selector and returns the html string from the
-- set of tags described by the given selector.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'htmls'.
html :: (TagSoup.StringLike str, Monad m) => Selector -> ScraperT str m str
html s = MkScraper $ withHead tagsToHTML =<< reader (select s)

-- | The 'htmls' function takes a selector and returns the html string from
-- every set of tags (possibly nested) matching the given selector.
--
-- > s = "<div><div>A</div></div>"
-- > scrapeStringLike s (htmls "div") == Just ["<div><div>A</div></div>", "<div>A</div>"]
htmls :: (TagSoup.StringLike str, Monad m)
      => Selector -> ScraperT str m [str]
htmls s = MkScraper $ withAll tagsToHTML =<< reader (select s)

-- | The 'innerHTML' function takes a selector and returns the inner html string
-- from the set of tags described by the given selector. Inner html here meaning
-- the html within but not including the selected tags.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'innerHTMLs'.
innerHTML :: (TagSoup.StringLike str, Monad m)
          => Selector -> ScraperT str m str
innerHTML s = MkScraper $ withHead tagsToInnerHTML =<< reader (select s)

-- | The 'innerHTMLs' function takes a selector and returns the inner html
-- string from every set of tags (possibly nested) matching the given selector.
--
-- > s = "<div><div>A</div></div>"
-- > scrapeStringLike s (innerHTMLs "div") == Just ["<div>A</div>", "A"]
innerHTMLs :: (TagSoup.StringLike str, Monad m)
           => Selector -> ScraperT str m [str]
innerHTMLs s = MkScraper $ withAll tagsToInnerHTML =<< reader (select s)

-- | The 'attr' function takes an attribute name and a selector and returns the
-- value of the attribute of the given name for the first opening tag that
-- matches the given selector.
--
-- This function will match only the opening tag matching the selector, to match
-- every tag, use 'attrs'.
attr :: (Show str, TagSoup.StringLike str, Monad m)
     => String -> Selector -> ScraperT str m str
attr name s = MkScraper $ ReaderT $ MaybeT
              . return . listToMaybe . catMaybes
              . fmap (tagsToAttr $ TagSoup.castString name) . select s

-- | The 'attrs' function takes an attribute name and a selector and returns the
-- value of the attribute of the given name for every opening tag
-- (possibly nested) that matches the given selector.
--
-- > s = "<div id=\"out\"><div id=\"in\"></div></div>"
-- > scrapeStringLike s (attrs "id" "div") == Just ["out", "in"]
attrs :: (Show str, TagSoup.StringLike str, Monad m)
     => String -> Selector -> ScraperT str m [str]
attrs name s = MkScraper $ ReaderT $ MaybeT
               . return . Just . catMaybes
               . fmap (tagsToAttr nameStr) . select s
    where nameStr = TagSoup.castString name

-- | The 'position' function is intended to be used within the do-block of a
-- `chroots` call. Within the do-block position will return the index of the
-- current sub-tree within the list of all sub-trees matched by the selector
-- passed to `chroots`.
--
-- For example, consider the following HTML:
--
-- @
-- \<article\>
--  \<p\> First paragraph. \</p\>
--  \<p\> Second paragraph. \</p\>
--  \<p\> Third paragraph. \</p\>
-- \</article\>
-- @
--
-- The `position` function can be used to determine the index of each @\<p\>@ tag
-- within the @article@ tag by doing the following.
--
-- @
-- chroots "article" // "p" $ do
--   index   <- position
--   content <- text "p"
--   return (index, content)
-- @
--
-- Which will evaluate to the list:
--
-- @
-- [
--   (0, "First paragraph.")
-- , (1, "Second paragraph.")
-- , (2, "Third paragraph.")
-- ]
-- @
position :: (TagSoup.StringLike str, Monad m) => ScraperT str m Int
position = MkScraper $ reader tagsToPosition

withHead :: Monad m => (a -> b) -> [a] -> ReaderT (TagSpec str) (MaybeT m) b
withHead _ []    = empty
withHead f (x:_) = return $ f x

withAll :: Monad m => (a -> b) -> [a] -> ReaderT (TagSpec str) (MaybeT m) [b]
withAll f xs = return $ map f xs

foldSpec :: TagSoup.StringLike str
         => (TagSoup.Tag str -> str -> str) -> TagSpec str -> str
foldSpec f = Vector.foldr' (f . infoTag) TagSoup.empty . (\(a, _, _) -> a)


tagsToText :: TagSoup.StringLike str => TagSpec str -> str
tagsToText = foldSpec f
    where
        f (TagSoup.TagText str) s = str `TagSoup.append` s
        f _                     s = s

tagsToHTML :: TagSoup.StringLike str => TagSpec str -> str
tagsToHTML = foldSpec (\tag s -> TagSoup.renderTags [tag] `TagSoup.append` s)

tagsToInnerHTML :: TagSoup.StringLike str => TagSpec str -> str
tagsToInnerHTML (tags, tree, ctx)
    | len < 2   = TagSoup.empty
    | otherwise = tagsToHTML (Vector.slice 1 (len - 2) tags, tree, ctx)
    where len = Vector.length tags

tagsToAttr :: (Show str, TagSoup.StringLike str)
           => str -> TagSpec str -> Maybe str
tagsToAttr tagName (tags, _, _) = do
    guard $ 0 < Vector.length tags
    let tag = infoTag $ tags Vector.! 0
    guard $ TagSoup.isTagOpen tag
    return $ TagSoup.fromAttrib tagName tag

tagsToPosition :: TagSpec str -> Int
tagsToPosition (_, _, ctx) = ctxPosition ctx
