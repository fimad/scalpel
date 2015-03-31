{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.URL (
    URL
,   scrapeURL
,   scrapeURLWithOpts
) where

import Text.HTML.Scalpel.Internal.Scrape

import Control.Applicative

import qualified Network.Curl as Curl
import qualified Network.Curl.Download as Curl
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


type URL = String

-- | The 'scrapeURL' function downloads the contents of the given URL and
-- executes a 'Scraper' on it.
scrapeURL :: TagSoup.StringLike str => URL -> Scraper str a -> IO (Maybe a)
scrapeURL = scrapeURLWithOpts [Curl.CurlFollowLocation True]

-- | The 'scrapeURLWithOpts' function take a list of curl options and downloads
-- the contents of the given URL and executes a 'Scraper' on it.
scrapeURLWithOpts :: TagSoup.StringLike str
                  => [Curl.CurlOption] -> URL -> Scraper str a -> IO (Maybe a)
scrapeURLWithOpts options url scraper = do
    maybeTags <- downloadAsTags url
    return (maybeTags >>= scrape scraper)
    where
        downloadAsTags url = do
            maybeBytes <- maybeRight <$> Curl.openURIWithOpts options url
            return $ (TagSoup.parseTags . TagSoup.castString) <$> maybeBytes
        maybeRight (Right a) = Just a
        maybeRight _         = Nothing
