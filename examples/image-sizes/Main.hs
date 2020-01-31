{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State as ST

import Data.ByteString.Char8 as BS (unpack)

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.URI

import System.Environment

import Text.HTML.Scalpel
import Text.Read (readMaybe)

-- Metadata containing filesize when possible
data Meta = Meta (Maybe Int)
  deriving Show

data Image = Image String Meta
  deriving Show

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = do tags <- fetchTags url
                      imgs <- evalStateT (scrapeT (scrapeImages url) tags) []
                      mapM_ (mapM_ print) imgs
handleArgs _     = putStrLn "usage: image-sizes URL"

-- Scrape the page for images: get their metadata
-- 'StateT [String]' allows us to store already-visited nodes
scrapeImages :: URL -> ScraperT String (StateT [String] IO) [Image]
scrapeImages topUrl = do
    chroots "img" $ do
        source <- attr "src" "img"
        presentSources <- get
        -- ignore non-links, ignore already-visited links
        guard (not (null source) && not (source `elem` presentSources))
        -- insert new source in our state
        modify (source :)
        -- getImageMeta is called via liftIO because ScrapeT transforms over IO
        liftM (Image source) $ liftIO (getImageMeta topUrl source)

-- Parse a link that might be absolute or relative
parseLink :: String -> String -> String
parseLink topUrl maybeRelURL =
  maybe maybeRelURL show
    $ liftM2 relativeTo (parseRelativeReference maybeRelURL) (parseURI topUrl)

-- | Make a HEAD request to obtain file size
getImageMeta :: String -> String -> IO Meta
getImageMeta topUrl source = do
  request <- parseRequest (parseLink topUrl source)
  globalManager <- getGlobalManager
  headers <- withResponse request globalManager (return . responseHeaders)
  let maybeLengthByteString = lookup "Content-Length" headers
      maybeLength = readMaybe =<< fmap BS.unpack maybeLengthByteString
  return (Meta maybeLength)
