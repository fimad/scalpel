{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.URL (
    URL
,   scrapeURL
,   scrapeURLWithOpts
) where

import Text.HTML.Scalpel.Internal.Scrape

import Control.Applicative

import qualified Data.ByteString as BS
import qualified Network.Curl as Curl
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
            maybeBytes <- openURIWithOpts url options
            return $ (TagSoup.parseTags . TagSoup.castString) <$> maybeBytes

openURIWithOpts :: URL -> [Curl.CurlOption] -> IO (Maybe BS.ByteString)
openURIWithOpts url opts = do
    resp <- curlGetResponse_ url opts
    return $ if Curl.respCurlCode resp /= Curl.CurlOK
        then Nothing
        else Just $ Curl.respBody resp

curlGetResponse_ :: URL
                 -> [Curl.CurlOption]
                 -> IO (Curl.CurlResponse_ [(String, String)] BS.ByteString)
curlGetResponse_ = Curl.curlGetResponse_
