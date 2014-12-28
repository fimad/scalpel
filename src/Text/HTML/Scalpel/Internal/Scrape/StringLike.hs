{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.StringLike (
    scrapeStringLike
) where

import Text.HTML.Scalpel.Internal.Scrape

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


-- | The 'scrapeStringLike' function parses a 'StringLike' value into a list of
-- tags and executes a 'Scraper' on it.
scrapeStringLike :: TagSoup.StringLike str => str -> Scraper str a -> Maybe a
scrapeStringLike html scraper = scrape scraper (TagSoup.parseTags html)
