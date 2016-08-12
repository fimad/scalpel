{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel


exampleHtml :: String
exampleHtml = "<html>\
\    <body>\
\        <div class='comments'>\
\            <div class='comment container'>\
\                <span class='comment author'>Sally</span>\
\                <div class='comment text'>Woo hoo!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bill</span>\
\                <img alt='A cat picture.' \
\                     class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bill</span>\
\                <img alt='A dog picture.' \
\                     class='comment image' src='http://example.com/dog.gif' />\
\            </div>\
\        </div>\
\    </body>\
\</html>"

main :: IO ()
main = print $ scrapeStringLike exampleHtml altTextAndImages

altTextAndImages :: Scraper String [(String, URL)]
altTextAndImages =
    -- 1. First narrow the current context to each img tag.
    chroots "img" $ do
        -- 2. Use Any to access all the relevant content from the the currently
        -- selected img tag.
        altText <- attr "alt" anySelector
        srcUrl  <- attr "src" anySelector
        -- 3. Combine the retrieved content into the desired final result.
        return (altText, srcUrl)
