{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Strict (Writer, runWriter)


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
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment video'>A video? That's new!</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"

type Error = String

type Author = String

data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

type ScraperWithError a = ScraperT String (Writer [Error]) a

scrapeStringOrError :: String -> ScraperWithError a -> (Maybe a, [Error])
scrapeStringOrError html scraper = runWriter $ scrapeStringLikeT html scraper

main :: IO ()
main = print $ scrapeStringOrError exampleHtml comments
    where
    comments :: ScraperWithError [Comment]
    comments = chroots ("div" @: [hasClass "container"]) comment

    logError :: String -> ScraperWithError a
    logError message = do
        currentHtml <- html anySelector
        tell [message ++ currentHtml]
        empty

    comment :: ScraperWithError Comment
    comment = textComment <|> imageComment <|> logError "Unknown comment type: "

    textComment :: ScraperWithError Comment
    textComment = do
        author      <- text $ "span" @: [hasClass "author"]
        commentText <- text $ "div"  @: [hasClass "text"]
        return $ TextComment author commentText

    imageComment :: ScraperWithError Comment
    imageComment = do
        author   <- text       $ "span" @: [hasClass "author"]
        imageURL <- attr "src" $ "img"  @: [hasClass "image"]
        return $ ImageComment author imageURL
