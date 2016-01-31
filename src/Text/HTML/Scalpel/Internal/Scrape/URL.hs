{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.URL (
    URL
,   Config (..)
,   Decoder

,   defaultDecoder
,   utf8Decoder
,   iso88591Decoder

,   scrapeURL
,   scrapeURLWithOpts
,   scrapeURLWithConfig
) where

import Text.HTML.Scalpel.Internal.Scrape

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.Default (def)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)

import qualified Data.ByteString as BS
import qualified Data.Default as Default
import qualified Data.Text.Encoding as Text
import qualified Network.Curl as Curl
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


type URL = String

type CurlResponse = Curl.CurlResponse_ [(String, String)] BS.ByteString

-- | A method that takes a HTTP response as raw bytes and returns the body as a
-- string type.
type Decoder str = Curl.CurlResponse_ [(String, String)] BS.ByteString -> str

-- | A record type that determines how 'scrapeUrlWithConfig' interacts with the
-- HTTP server and interprets the results.
data Config str = Config {
    curlOpts :: [Curl.CurlOption]
,   decoder  :: Decoder str
}

instance TagSoup.StringLike str => Default.Default (Config str) where
    def = Config {
            curlOpts = [Curl.CurlFollowLocation True]
        ,   decoder  = defaultDecoder
        }

-- | The 'scrapeURL' function downloads the contents of the given URL and
-- executes a 'Scraper' on it.
scrapeURL :: (Ord str, TagSoup.StringLike str)
          => URL -> Scraper str a -> IO (Maybe a)
scrapeURL = scrapeURLWithOpts [Curl.CurlFollowLocation True]

-- | The 'scrapeURLWithOpts' function take a list of curl options and downloads
-- the contents of the given URL and executes a 'Scraper' on it.
scrapeURLWithOpts :: (Ord str, TagSoup.StringLike str)
                  => [Curl.CurlOption] -> URL -> Scraper str a -> IO (Maybe a)
scrapeURLWithOpts options = scrapeURLWithConfig (def {curlOpts = options})

-- | The 'scrapeURLWithConfig' function takes a 'Config' record type and
-- downloads the contents of the given URL and executes a 'Scraper' on it.
scrapeURLWithConfig :: (Ord str, TagSoup.StringLike str)
                  => Config str -> URL -> Scraper str a -> IO (Maybe a)
scrapeURLWithConfig config url scraper = do
    maybeTags <- downloadAsTags (decoder config) url
    return (maybeTags >>= scrape scraper)
    where
        downloadAsTags decoder url = do
            maybeBytes <- openURIWithOpts url (curlOpts config)
            return $ TagSoup.parseTags . decoder <$> maybeBytes

openURIWithOpts :: URL -> [Curl.CurlOption] -> IO (Maybe CurlResponse)
openURIWithOpts url opts = do
    resp <- curlGetResponse_ url opts
    return $ if Curl.respCurlCode resp /= Curl.CurlOK
        then Nothing
        else Just resp

curlGetResponse_ :: URL
                 -> [Curl.CurlOption]
                 -> IO (Curl.CurlResponse_ [(String, String)] BS.ByteString)
curlGetResponse_ = Curl.curlGetResponse_

-- | The default response decoder. This decoder attempts to infer the character
-- set of the HTTP response body from the `Content-Type` header. If this header
-- is not present, then the character set is assumed to be `ISO-8859-1`.
defaultDecoder :: TagSoup.StringLike str => Decoder str
defaultDecoder response = TagSoup.castString
                        $ choosenDecoder body
    where
        body        = Curl.respBody response
        headers     = Curl.respHeaders response
        contentType = listToMaybe
                    $ map (map toLower . snd)
                    $ take 1
                    $ dropWhile ((/= "content-type") . map toLower . fst)
                                headers

        isType t | Just ct <- contentType = ("charset=" ++ t) `isInfixOf` ct
                 | otherwise              = False

        choosenDecoder | isType "utf-8" = Text.decodeUtf8
                       | otherwise      = Text.decodeLatin1

-- | A decoder that will always decode using `UTF-8`.
utf8Decoder ::  TagSoup.StringLike str => Decoder str
utf8Decoder = TagSoup.castString . Text.decodeUtf8 . Curl.respBody

-- | A decoder that will always decode using `ISO-8859-1`.
iso88591Decoder ::  TagSoup.StringLike str => Decoder str
iso88591Decoder = TagSoup.castString . Text.decodeLatin1 . Curl.respBody
