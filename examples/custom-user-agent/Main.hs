{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Default (def)
import System.Environment
import Text.HTML.Scalpel

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP


-- Create a new manager settings based on the default TLS manager that updates
-- the request headers to include a custom user agent.
managerSettings :: HTTP.ManagerSettings
managerSettings = HTTP.tlsManagerSettings {
  HTTP.managerModifyRequest = \req -> do
    req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
    return $ req' {
      HTTP.requestHeaders = (HTTP.hUserAgent, "My Custom UA")
                          : HTTP.requestHeaders req'
    }
}

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = listUrlsForSite url
handleArgs _     = putStrLn "usage: custom-user-agent URL"

listUrlsForSite :: URL -> IO ()
listUrlsForSite url = do
    manager <- Just <$> HTTP.newManager managerSettings
    images <- scrapeURLWithConfig (def { manager }) url (attrs "src" "img")
    maybe printError printImages images
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printImages = mapM_ putStrLn
