{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Data.List (isInfixOf)


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
\                <img class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bertrand</span>\
\                <div class='comment text'>That sure is some cat!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"

main :: IO ()
main = print $ scrapeStringLike exampleHtml catComment

catComment :: Scraper String String
catComment =
    -- 1. First narrow the current context to the div containing the comment's
    --    textual content.
    chroot ("div" @: [hasClass "comment", hasClass "text"]) $ do
        -- 2. Any can be used to access the root tag of the current context.
        contents <- text anySelector
        -- 3. Skip comment divs that do not contain "cat".
        guard ("cat" `isInfixOf` contents)
        -- 4. Generate the desired value.
        html anySelector
