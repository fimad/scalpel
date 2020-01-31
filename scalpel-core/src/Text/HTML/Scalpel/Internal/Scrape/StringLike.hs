{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.StringLike (
    scrapeStringLike
,   scrapeStringLikeT
) where

import Data.Functor.Identity
import Text.HTML.Scalpel.Internal.Scrape

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup

-- | The 'scrapeStringLike' function parses a 'StringLike' value into a list of
-- tags and executes a 'Scraper' on it.
scrapeStringLike :: (TagSoup.StringLike str)
                 => str -> Scraper str a -> Maybe a
scrapeStringLike = fmap runIdentity . scrapeStringLikeT

-- | The 'scrapeStringLikeT' function parses a 'StringLike' value into a list of
-- tags and executes a 'ScraperT' on it. Since 'ScraperT' is a monad
-- transformer, the result is monadic.
scrapeStringLikeT :: (TagSoup.StringLike str, Monad m)
                  => str -> ScraperT str m a -> m (Maybe a)
scrapeStringLikeT tags scraper = scrapeT scraper
    (TagSoup.parseTagsOptions TagSoup.parseOptionsFast tags)
