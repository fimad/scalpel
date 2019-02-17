-- A simplistic example of how to (imperfectly) convert an HTML document into
-- the rough markdown equivalent using serial scrapers.

{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import System.Environment
import Text.HTML.Scalpel
import Data.List

import qualified Data.Text as T
import qualified Data.Text.IO as T


-- A data type that will hold the scraped text along with markdown relevant
-- styling.
data FormatedText
    = PlainText T.Text
    | PlainTexts [FormatedText]
    | Header Int [FormatedText]
    | Paragraph [FormatedText]
    | Bold [FormatedText]
    | Italic [FormatedText]
    | Link T.Text [FormatedText]
    | NewLine

type Markdown = T.Text

-- Escape some characters that are known to cause issues in markdown.
escapeMd :: T.Text -> Markdown
escapeMd = T.replace "\n" ""
         . T.replace "\\" "\\\\"
         . T.replace "*" "\\*"
         . T.replace "_" "\\_"
         . T.replace ">" "&gt;"

mdFromFormatedText :: FormatedText -> Markdown
mdFromFormatedText NewLine         = "\n\n"
mdFromFormatedText (PlainText t)   = escapeMd $ T.strip t
mdFromFormatedText (PlainTexts cs) = mdFromFormatedTexts cs
mdFromFormatedText (Paragraph cs)  = mdFromFormatedTexts cs `T.append` "\n\n"
mdFromFormatedText (Bold cs)       = decorate "**" cs
mdFromFormatedText (Italic cs)     = decorate "*" cs
mdFromFormatedText (Link url cs)   =
    T.concat ["[" , mdFromFormatedTexts cs, "](", url, ")"]
mdFromFormatedText (Header n cs)   =
    T.concat ["\n", T.replicate n "#", " ", mdFromFormatedTexts cs, "\n\n"]

mdFromFormatedTexts :: [FormatedText] -> Markdown
mdFromFormatedTexts = T.concat . intersperse " " . map mdFromFormatedText

-- Pre- and appends a textual decoration to the markdown generated from the
-- given formated text.
decorate :: T.Text -> [FormatedText] -> Markdown
decorate decoration cs = T.concat [
    decoration
  , mdFromFormatedTexts cs
  , decoration
  ]

formatedText :: Scraper T.Text FormatedText
formatedText = PlainTexts <$> formatedTexts

formatedTexts :: Scraper T.Text [FormatedText]
formatedTexts = inSerial $ many $ stepNext innerScraper
  where
    innerScraper = formatting <|> link <|> headers <|> skip <|> unknown

    formatting =   newLine <|> plainText <|> paragraph <|> bold <|> italic
               <|> header
    newLine    = NewLine    <$ matches ("br" `atDepth` 0)
    plainText  = PlainText  <$> text (textSelector `atDepth` 0)
    paragraph  = Paragraph  <$> recurseOn "p"
    header     = Paragraph  <$> recurseOn "header"
    bold       = Bold       <$> recurseOn "b"
    italic     = Italic     <$> recurseOn "em"

    link = chroot ("a" `atDepth` 0)
           (Link <$> attr "href" anySelector <*> formatedTexts)

    headers = h1 <|> h2 <|> h3 <|> h4 <|> h5 <|> h6
    h1 = Header 1 <$> recurseOn "h1"
    h2 = Header 2 <$> recurseOn "h2"
    h3 = Header 3 <$> recurseOn "h3"
    h4 = Header 4 <$> recurseOn "h4"
    h5 = Header 5 <$> recurseOn "h5"
    h6 = Header 6 <$> recurseOn "h6"

    skip     = noscript <|> script <|> nav
    script   = PlainTexts [] <$ recurseOn "script"
    noscript = PlainTexts [] <$ recurseOn "noscript"
    nav      = PlainTexts [] <$ recurseOn "nav"

    unknown   = PlainTexts <$> recurseOn anySelector

    recurseOn tag = chroot (tag `atDepth` 0) formatedTexts

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs [url] = urlToMd url
handleArgs _     = putStrLn "usage: html-to-markdown URL"

urlToMd :: URL -> IO ()
urlToMd url = do
    c <- scrapeURL url (
            -- Prefer to extract just the article content.
            chroot "article" formatedText
            -- And fall back to everything in the body otherwise.
        <|> chroot "body" formatedText)
    maybe printError printMd c
    where
        printError = putStrLn "ERROR: Could not scrape the URL!"
        printMd    = T.putStrLn . cleanup . mdFromFormatedText

        -- Cleanup some whitespace left over from converting to markdown.
        -- TODO: Ideally this should be part of the mdFromFormatedText function.
        cleanup = removeIndents . collapseNewLines . T.strip

        removeIndents t | t == t' = t
                        | otherwise = removeIndents t'
          where t' = T.replace "\n " "\n" t

        collapseNewLines t | t == t' = t
                           | otherwise = collapseNewLines t'
          where t' = T.replace "\n\n\n\n" "\n\n" t
