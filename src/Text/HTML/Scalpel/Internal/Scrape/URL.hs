{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.URL (
    URL
,   scrapeURL
) where

import Text.HTML.Scalpel.Internal.Scrape

import Control.Applicative

import qualified Network.Curl.Download as Curl
import qualified Text.StringLike as TagSoup


type URL = String

-- | The 'scrapeURL' function downloads the contents of the given URL and
-- executes a 'Scraper' on it.
scrapeURL :: TagSoup.StringLike str => URL -> Scraper str a -> IO (Maybe a)
scrapeURL url scraper = (eitherToMaybeA . toStr) <$> Curl.openAsTags url
    where eitherToMaybeA = either (const Nothing) (scrape scraper)
          toStr = (((TagSoup.castString <$>) <$>) <$>)
