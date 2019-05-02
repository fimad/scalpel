{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape (
    Scraper (..)
,   scrape
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
import Data.Maybe
import Text.StringLike (StringLike, castString)

import qualified Control.Monad.Fail as Fail
import qualified Data.Vector as Vector
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


-- | A value of 'Scraper' @a@ defines a web scraper that is capable of consuming
-- a list of 'TagSoup.Tag's and optionally producing a value of type @a@.
newtype Scraper str a = MkScraper {
        scrapeTagSpec :: TagSpec str -> Maybe a
    }

instance Functor (Scraper str) where
    fmap f (MkScraper a) = MkScraper $ fmap (fmap f) a

instance Applicative (Scraper str) where
    pure = MkScraper . const . Just
    (MkScraper f) <*> (MkScraper a) = MkScraper applied
        where applied tags | (Just aVal) <- a tags = ($ aVal) <$> f tags
                           | otherwise             = Nothing

instance Alternative (Scraper str) where
    empty = MkScraper $ const Nothing
    (MkScraper a) <|> (MkScraper b) = MkScraper choice
        where choice tags | (Just aVal) <- a tags = Just aVal
                          | otherwise             = b tags

instance Monad (Scraper str) where
    fail = Fail.fail
    return = pure
    (MkScraper a) >>= f = MkScraper combined
        where combined tags | (Just aVal) <- a tags = let (MkScraper b) = f aVal
                                                      in  b tags
                            | otherwise             = Nothing

instance MonadPlus (Scraper str) where
    mzero = empty
    mplus = (<|>)

instance Fail.MonadFail (Scraper str) where
    fail _ = mzero

-- | The 'scrape' function executes a 'Scraper' on a list of
-- 'TagSoup.Tag's and produces an optional value.
scrape :: (StringLike str)
       => Scraper str a -> [TagSoup.Tag str] -> Maybe a
scrape s = scrapeTagSpec s . tagsToSpec . TagSoup.canonicalizeTags

-- | The 'chroot' function takes a selector and an inner scraper and executes
-- the inner scraper as if it were scraping a document that consists solely of
-- the tags corresponding to the selector.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'chroots'.
chroot :: (StringLike str)
       => Selector -> Scraper str a -> Scraper str a
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
chroots :: (StringLike str)
        => Selector -> Scraper str a -> Scraper str [a]
chroots selector (MkScraper inner) = MkScraper
                                   $ return . mapMaybe inner . select selector

-- | The 'matches' function takes a selector and returns `()` if the selector
-- matches any node in the DOM.
matches :: (StringLike str) => Selector -> Scraper str ()
matches s = MkScraper $ withHead (pure ()) . select s

-- | The 'text' function takes a selector and returns the inner text from the
-- set of tags described by the given selector.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'texts'.
text :: (StringLike str) => Selector -> Scraper str str
text s = MkScraper $ withHead tagsToText . select s

-- | The 'texts' function takes a selector and returns the inner text from every
-- set of tags (possibly nested) matching the given selector.
--
-- > s = "<div>Hello <div>World</div></div>"
-- > scrapeStringLike s (texts "div") == Just ["Hello World", "World"]
texts :: (StringLike str)
      => Selector -> Scraper str [str]
texts s = MkScraper $ withAll tagsToText . select s

-- | The 'html' function takes a selector and returns the html string from the
-- set of tags described by the given selector.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'htmls'.
html :: (StringLike str) => Selector -> Scraper str str
html s = MkScraper $ withHead tagsToHTML . select s

-- | The 'htmls' function takes a selector and returns the html string from
-- every set of tags (possibly nested) matching the given selector.
--
-- > s = "<div><div>A</div></div>"
-- > scrapeStringLike s (htmls "div") == Just ["<div><div>A</div></div>", "<div>A</div>"]
htmls :: (StringLike str)
      => Selector -> Scraper str [str]
htmls s = MkScraper $ withAll tagsToHTML . select s

-- | The 'innerHTML' function takes a selector and returns the inner html string
-- from the set of tags described by the given selector. Inner html here meaning
-- the html within but not including the selected tags.
--
-- This function will match only the first set of tags matching the selector, to
-- match every set of tags, use 'innerHTMLs'.
innerHTML :: (StringLike str)
          => Selector -> Scraper str str
innerHTML s = MkScraper $ withHead tagsToInnerHTML . select s

-- | The 'innerHTMLs' function takes a selector and returns the inner html
-- string from every set of tags (possibly nested) matching the given selector.
--
-- > s = "<div><div>A</div></div>"
-- > scrapeStringLike s (innerHTMLs "div") == Just ["<div>A</div>", "A"]
innerHTMLs :: (StringLike str)
           => Selector -> Scraper str [str]
innerHTMLs s = MkScraper $ withAll tagsToInnerHTML . select s

-- | The 'attr' function takes an attribute name and a selector and returns the
-- value of the attribute of the given name for the first opening tag that
-- matches the given selector.
--
-- This function will match only the opening tag matching the selector, to match
-- every tag, use 'attrs'.
attr :: (Show str, StringLike str)
     => str -> Selector -> Scraper str str
attr name s = MkScraper
            $ join . withHead (tagsToAttr $ castString name) . select s

-- | The 'attrs' function takes an attribute name and a selector and returns the
-- value of the attribute of the given name for every opening tag
-- (possibly nested) that matches the given selector.
--
-- > s = "<div id=\"out\"><div id=\"in\"></div></div>"
-- > scrapeStringLike s (attrs "id" "div") == Just ["out", "in"]
attrs :: (Show str, StringLike str)
     => str -> Selector -> Scraper str [str]
attrs name s = MkScraper
             $ fmap catMaybes . withAll (tagsToAttr nameStr) . select s
    where nameStr = castString name

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
position :: (StringLike str) => Scraper str Int
position = MkScraper $ Just . tagsToPosition

withHead :: (a -> b) -> [a] -> Maybe b
withHead _ []    = Nothing
withHead f (x:_) = Just $ f x

withAll :: (a -> b) -> [a] -> Maybe [b]
withAll f xs = Just $ map f xs

foldSpec :: StringLike str
         => (TagSoup.Tag str -> str -> str) -> TagSpec str -> str
foldSpec f = Vector.foldr' (f . infoTag) TagSoup.empty . (\(a, _, _) -> a)


tagsToText :: StringLike str => TagSpec str -> str
tagsToText = foldSpec f
    where
        f (TagSoup.TagText str) s = str `TagSoup.append` s
        f _                     s = s

tagsToHTML :: StringLike str => TagSpec str -> str
tagsToHTML = foldSpec (\tag s -> TagSoup.renderTags [tag] `TagSoup.append` s)

tagsToInnerHTML :: StringLike str => TagSpec str -> str
tagsToInnerHTML (tags, tree, ctx)
    | len < 2   = TagSoup.empty
    | otherwise = tagsToHTML (Vector.slice 1 (len - 2) tags, tree, ctx)
    where len = Vector.length tags

tagsToAttr :: (Show str, StringLike str)
           => str -> TagSpec str -> Maybe str
tagsToAttr attr (tags, _, _) = do
    guard $ 0 < Vector.length tags
    let tag = infoTag $ tags Vector.! 0
    guard $ TagSoup.isTagOpen tag
    return $ TagSoup.fromAttrib attr tag

tagsToPosition :: TagSpec str -> Int
tagsToPosition (_, _, ctx) = ctxPosition ctx
