{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.URL (
    URL
,   Config (..)
,   Decoder

,   defaultDecoder
,   utf8Decoder
,   iso88591Decoder

,   fetchTags
,   fetchTagsWithConfig
,   scrapeURL
,   scrapeURLWithConfig
) where

import Text.HTML.Scalpel.Core

import Control.Monad
import Data.CaseInsensitive ()
import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Default as Default
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Text.HTML.Parser as HP


type URL = String

-- | A method that takes a HTTP response as raw bytes and returns the body as a
-- string type.
type Decoder str = HTTP.Response LBS.ByteString -> str

-- | A record type that determines how 'scrapeURLWithConfig' interacts with the
-- HTTP server and interprets the results.
data Config = Config {
    decoder :: Decoder Text
,   manager :: Maybe HTTP.Manager
}

instance Default.Default Config where
    def = Config {
            decoder = defaultDecoder
        ,   manager = Nothing
        }

-- | The 'scrapeURL' function downloads the contents of the given URL and
-- executes a 'Scraper' on it.
--
-- The default behavior is to use the global manager provided by
-- http-client-tls (via 'HTTP.getGlobalManager'). Any exceptions thrown by
-- http-client are not caught and are bubbled up to the caller.
scrapeURL :: URL -> Scraper a -> IO (Maybe a)
scrapeURL = scrapeURLWithConfig def

-- | The 'scrapeURLWithConfig' function takes a 'Config' record type and
-- downloads the contents of the given URL and executes a 'Scraper' on it.
--scrapeURLWithConfig :: Config -> URL -> _-> IO (Maybe a)
scrapeURLWithConfig config url scraper = do
    (scrape scraper) `liftM` fetchTagsWithConfig config url

-- | Download and parse the contents of the given URL.
fetchTags :: URL -> IO [HP.Token]
fetchTags = fetchTagsWithConfig def

-- | Download and parse the contents of the given URL with the given 'Config'.
fetchTagsWithConfig :: Config -> URL -> IO [HP.Token]
fetchTagsWithConfig config url = do
    manager <- maybe HTTP.getGlobalManager return (manager config)
    response <- flip HTTP.httpLbs manager =<< HTTP.parseRequest url
    return $ HP.parseTokens $ decoder config $ response

-- | The default response decoder. This decoder attempts to infer the character
-- set of the HTTP response body from the `Content-Type` header. If this header
-- is not present, then the character set is assumed to be `ISO-8859-1`.
defaultDecoder :: Decoder Text
defaultDecoder response = choosenDecoder body
    where
        body        = HTTP.responseBody response
        headers     = HTTP.responseHeaders response
        contentType = listToMaybe
                    $ map (Text.toLower . Text.decodeLatin1 . snd)
                    $ take 1
                    $ dropWhile ((/= "content-type") . fst)
                                headers

        isType t | Just ct <- contentType = ("charset=" `Text.append` t) `Text.isInfixOf` ct
                 | otherwise              = False

        choosenDecoder | isType "utf-8" = Text.decodeUtf8 . LBS.toStrict
                       | otherwise      = Text.decodeLatin1 . LBS.toStrict

-- | A decoder that will always decode using `UTF-8`.
utf8Decoder :: Decoder Text
utf8Decoder = Text.decodeUtf8 . LBS.toStrict . HTTP.responseBody

-- | A decoder that will always decode using `ISO-8859-1`.
iso88591Decoder :: Decoder Text
iso88591Decoder = Text.decodeLatin1 . LBS.toStrict . HTTP.responseBody
