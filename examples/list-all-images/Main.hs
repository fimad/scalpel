{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.HTML.Scalpel


main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = listUrlsForSite url
handleArgs _     = putStrLn "usage: list-all-images URL"

listUrlsForSite :: URL -> IO ()
listUrlsForSite url = do
    images <- scrapeURL url (attrs "src" "img")
    maybe printError printImages images
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printImages = mapM_ putStrLn
