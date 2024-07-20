{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<|>))
import Text.HTML.Scalpel

htmlString :: String
htmlString =
    "<html>\
    \  <body>\
    \    <div class='comments'>\
    \      <div class='comment container'>\
    \        <span class='comment author'>Sally</span>\
    \        <div class='comment text'>Woo hoo!</div>\
    \      </div>\
    \      <div class='comment container'>\
    \        <span class='comment author'>Bill</span>\
    \        <img class='comment image' src='http://example.com/cat.gif' />\
    \      </div>\
    \    </div>\
    \  </body>\
    \</html>"

main :: IO ()
main = do
    -- We can either scrape a raw html of any StringLike type (fetched before by other means):
    let scrapedCommentsFromString = scrapeStringLike htmlString comments
    -- prints: Just [TextComment "Sally" "Woo hoo!",ImageComment "Bill" "http://example.com/cat.gif"]
    print scrapedCommentsFromString

    -- or let Scalpel fetch and scrape an HTML page for us for convenience :
    scrapedCommentsFromUrl <- scrapeURL "http://example.org/article.html" comments
    -- example.org doesn't have the HTML above
    -- prints: Just []
    print scrapedCommentsFromUrl

type Author = String

data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

comments :: Scraper String [Comment]
comments = chroots ("div" @: [hasClass "container"]) comment
  where
    comment :: Scraper String Comment
    comment = textComment <|> imageComment

    textComment :: Scraper String Comment
    textComment = do
        author <- text $ "span" @: [hasClass "author"]
        commentText <- text $ "div" @: [hasClass "text"]
        return $ TextComment author commentText

    imageComment :: Scraper String Comment
    imageComment = do
        author <- text $ "span" @: [hasClass "author"]
        imageURL <- attr "src" $ "img" @: [hasClass "image"]
        return $ ImageComment author imageURL
