{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.HTML.Scalpel.Internal.Scrape (
    Scraper (..)
,   attr
,   attrs
,   text
,   texts
,   chroot
,   chroots
) where

import Text.HTML.Scalpel.Internal.Select
import Text.HTML.Scalpel.Internal.Select.Types

import Control.Applicative
import Control.Monad
import Data.Maybe

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


newtype Scraper str a = MkScraper {scrape :: [TagSoup.Tag str] -> Maybe a}

instance Functor (Scraper str) where
    fmap f (MkScraper a) = MkScraper $ fmap (fmap f) a

instance Applicative (Scraper str) where
    pure = MkScraper . const . Just
    (MkScraper f) <*> (MkScraper a) = MkScraper applied
        where applied tags | (Just aVal) <- a tags = ($ aVal) <$> f tags
                           | otherwise             = Nothing

instance Monad (Scraper str) where
    return = pure
    (MkScraper a) >>= f = MkScraper combined
        where combined tags | (Just aVal) <- a tags = let (MkScraper b) = f aVal
                                                      in  b tags
                            | otherwise             = Nothing

-- scrapeURL :: TagSoup.StringLike str => URL -> Scraper str a -> Maybe a
-- scrapeURL = error "TODO"

-- | TODO: Document this.
chroot :: (TagSoup.StringLike str, Selectable str s)
       => s -> Scraper str a -> Scraper str a
chroot selector (MkScraper inner) = MkScraper
                                  $ join . (inner <$>)
                                  . listToMaybe . select selector

-- | TODO: Document this.
chroots :: (TagSoup.StringLike str, Selectable str s)
        => s -> Scraper str a -> Scraper str [a]
chroots selector (MkScraper inner) = MkScraper
                                   $ return . catMaybes
                                   . map inner . select selector

-- | TODO: Document this.
text :: (TagSoup.StringLike str, Selectable str s) => s -> Scraper str str
text s = MkScraper $ withHead tagsToText . select s

-- | TODO: Document this.
texts :: (TagSoup.StringLike str, Selectable str s) => s -> Scraper str [str]
texts s = MkScraper $ withAll tagsToText . select s

-- | TODO: Document this.
attr :: (Show str, TagSoup.StringLike str, Selectable str s)
     => str -> s -> Scraper str str
attr name s = MkScraper $ join . withHead (tagsToAttr name) . select s

-- | TODO: Document this.
attrs :: (Show str, TagSoup.StringLike str, Selectable str s)
     => str -> s -> Scraper str [str]
attrs name s = MkScraper $ fmap catMaybes . withAll (tagsToAttr name) . select s

withHead :: (a -> b) -> [a] -> Maybe b
withHead _ []    = Nothing
withHead f (x:_) = Just $ f x

withAll :: (a -> b) -> [a] -> Maybe [b]
withAll _ [] = Nothing
withAll f xs = Just $ map f xs

tagsToText :: TagSoup.StringLike str => [TagSoup.Tag str] -> str
tagsToText = TagSoup.innerText

tagsToAttr :: (Show str, TagSoup.StringLike str)
           => str -> [TagSoup.Tag str] -> Maybe str
tagsToAttr attr tags = do
    tag <- listToMaybe tags
    guard $ TagSoup.isTagOpen tag
    return $ TagSoup.fromAttrib attr tag
