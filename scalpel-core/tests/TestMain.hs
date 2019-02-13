{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.HTML.Scalpel.Core

import Control.Applicative
import Control.Monad (guard)
import Data.List (isInfixOf)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import Test.HUnit (Test(..), (@=?), (~:), runTestTT, failures)

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.Regex.TDFA

main :: IO ()
main = do
  n <- runTestTT (TestList [scrapeTests])
  exit $ failures n

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith $ ExitFailure n

re :: String -> Text.Regex.TDFA.Regex
re = Text.Regex.TDFA.makeRegex

scrapeTests = "scrapeTests" ~: TestList [
        scrapeTest
            "<a>foo</a>"
            (Just ["<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["<a>foo</a>", "<a>bar</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<b><a>foo</a></b>"
            (Just ["<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a><a>foo</a></a>"
            (Just ["<a><a>foo</a></a>", "<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a>foo</a>"
            (Just [])
            (htmls ("b" @: []))

    ,   scrapeTest
            "<a>foo"
            (Just ["<a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "<a>foo</a><a key=\"value\">bar</a>"
            (Just ["<a key=\"value\">bar</a>"])
            (htmls ("a" @: ["key" @= "value"]))

    ,   scrapeTest
            "<a><b><c>foo</c></b></a>"
            (Just ["<c>foo</c>"])
            (htmls ("a" // "b" @: [] // "c"))

    ,   scrapeTest
            "<c><a><b>foo</b></a></c><c><a><d><b>bar</b></d></a></c><b>baz</b>"
            (Just ["<b>foo</b>", "<b>bar</b>"])
            (htmls ("a" // "b"))

    ,   scrapeTest
            "<a class=\"a b\">foo</a>"
            (Just ["<a class=\"a b\">foo</a>"])
            (htmls ("a" @: [hasClass "a"]))

    ,   scrapeTest
            "<a class=\"a b\">foo</a>"
            (Just [])
            (htmls ("a" @: [hasClass "c"]))

    , scrapeTest
            "<a>foo</a><a class=\"a b\">bar</a><a class=\"b\">baz</a>"
            (Just ["foo", "baz"])
            (texts ("a" @: [notP $ hasClass "a"]))

    ,   scrapeTest
            "<a key=\"value\">foo</a>"
            (Just ["<a key=\"value\">foo</a>"])
            (htmls ("a" @: ["key" @=~ re "va(foo|bar|lu)e"]))

    ,   scrapeTest
            "<a foo=\"value\">foo</a><a bar=\"value\">bar</a>"
            (Just ["<a foo=\"value\">foo</a>", "<a bar=\"value\">bar</a>"])
            (htmls ("a" @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "<a foo=\"other\">foo</a><a bar=\"value\">bar</a>"
            (Just ["<a bar=\"value\">bar</a>"])
            (htmls ("a" @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "<a foo=\"value\">foo</a><b bar=\"value\">bar</b>"
            (Just ["<a foo=\"value\">foo</a>", "<b bar=\"value\">bar</b>"])
            (htmls (AnyTag @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "<a foo=\"other\">foo</a><b bar=\"value\">bar</b>"
            (Just ["<b bar=\"value\">bar</b>"])
            (htmls (AnyTag @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "<a foo=\"bar\">1</a><a foo=\"foo\">2</a><a bar=\"bar\">3</a>"
            (Just ["<a foo=\"foo\">2</a>", "<a bar=\"bar\">3</a>"])
            (htmls (AnyTag @: [match (==)]))

    ,   scrapeTest
            "<a>foo</a>"
            (Just "foo")
            (text "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just "foo")
            (text "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["foo", "bar"])
            (texts "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just [True, False])
            (map (== "foo") <$> texts "a")

    ,   scrapeTest
            "<a key=foo />"
            (Just "foo")
            (attr "key" "a")

    ,   scrapeTest
            "<a key1=foo/><b key1=bar key2=foo /><a key1=bar key2=baz />"
            (Just "baz")
            (attr "key2" $ "a" @: ["key1" @= "bar"])

    ,   scrapeTest
            "<a><b>foo</b></a><b>bar</b>"
            (Just ["foo"])
            (chroot "a" $ texts "b")

    ,   scrapeTest
            "<a><b>foo</b></a><a><b>bar</b></a>"
            (Just ["foo", "bar"])
            (chroots "a" $ text "b")

    ,   scrapeTest
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "foo")
            (text ("a" // "b") <|> text ("a" // "c"))

    ,   scrapeTest
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "bar")
            (text ("a" // "d") <|> text ("a" // "c"))

    ,   scrapeTest
            "<img src='foobar'>"
            (Just "foobar")
            (attr "src" "img")

    ,   scrapeTest
            "<img src='foobar' />"
            (Just "foobar")
            (attr "src" "img")

    ,   scrapeTest
            "<a>foo</a><A>bar</A>"
            (Just ["foo", "bar"])
            (texts "a")

    ,   scrapeTest
            "<a>foo</a><A>bar</A>"
            (Just ["foo", "bar"])
            (texts "A")

    ,   scrapeTest
            "<a B=C>foo</a>"
            (Just ["foo"])
            (texts $ "A" @: ["b" @= "C"])

    ,   scrapeTest
            "<a B=C>foo</a>"
            (Just [])
            (texts $ "A" @: ["b" @= "c"])

    , scrapeTest
            "<a>foo</a><a B=C>bar</a><a B=D>baz</a>"
            (Just ["foo", "baz"])
            (texts ("a" @: [notP $ "b" @= "C"]))

    ,   scrapeTest
            "<a>foo</a>"
            (Just "<a>foo</a>")
            (html "a")

    ,   scrapeTest
            "<body><div><ul><li>1</li><li>2</li></ul></div></body>"
            (Just "<li>1</li>")
            (html "li")

    ,   scrapeTest
            "<body><div></div></body>"
            (Just "<div></div>")
            (html "div")

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["<a>foo</a>","<a>bar</a>"])
            (htmls "a")

    ,   scrapeTest
            "<body><div><ul><li>1</li><li>2</li></ul></div></body>"
            (Just ["<li>1</li>", "<li>2</li>"])
            (htmls "li")

    ,   scrapeTest
            "<body><div></div></body>"
            (Just ["<div></div>"])
            (htmls "div")

    ,   scrapeTest
            "<a>1<b>2</b>3</a>"
            (Just "1<b>2</b>3")
            (innerHTML anySelector)

    ,   scrapeTest
            "<a>"
            (Just "")
            (innerHTML anySelector)

    ,   scrapeTest
            "<a>foo</a><a>bar</a>"
            (Just ["foo","bar"])
            (innerHTMLs "a")

    ,   scrapeTest
            "<a>foo</a><a>bar</a><a>baz</a>"
            (Just "<a>bar</a>")
            (chroot "a" $ do
                t <- text anySelector
                guard ("b" `isInfixOf` t)
                html anySelector)

    ,   scrapeTest
            "<div id=\"outer\"><div id=\"inner\">inner text</div></div>"
            (Just ["inner"])
            (attrs "id" ("div" // "div"))

    ,   scrapeTest
            "<div id=\"a\"><div id=\"b\"><div id=\"c\"></div></div></div>"
            (Just ["b", "c"])
            (attrs "id" ("div" // "div"))

    ,   scrapeTest
            "<a>1<b>2<c>3</c>4</b>5</a>"
            (Just "12345")
            (text anySelector)

    ,   scrapeTest
            "<a>1</a>"
            Nothing
            $ do
                "Bad pattern" <- text "a"
                return "OK"

    ,   scrapeTest
            "<a>1</a>"
            (Just "OK")
            $ do
                "1" <- text "a"
                return "OK"
    ,   scrapeTest
            "<article><p>A</p><p>B</p><p>C</p></article>"
            (Just [(0, "A"), (1, "B"), (2, "C")])
            (chroots ("article" // "p") $ do
                index   <- position
                content <- text anySelector
                return (index, content))

    ,   scrapeTest
            "<article><p>A</p></article><article><p>B</p><p>C</p></article>"
            (Just [[(0, "A")], [(0, "B"), (1, "C")]])
            (chroots "article" $ chroots "p" $ do
                index   <- position
                content <- text anySelector
                return (index, content))

    ,   scrapeTest
            "<div><p>p1</p><p>p2</p><blockquote><p>p3</p></blockquote><p>p4</p>"
            (Just ["p1", "p2", "p3", "p4"])
            (texts "p")

    ,   scrapeTest
            "<a><b>1</b></a><a><b>2</b></a><a><b>3</b></a>"
            (Just ["1","2","3"])
            (texts "a")

    ,   scrapeTest
            "<a><b>1</b></a><a><b>2</b></a><a><b>3</b></a>"
            (Just ["1","2","3"])
            (texts $ "a" // "b")

    ,   scrapeTest
            "<a><b>1</b></a><a><b>2</b></a><a><b>3</b></a>"
            (Just ["1","2","3"])
            (texts "b")

    ,   scrapeTest
            "<a><b>1</b><c><b>2</b></c></a>"
            (Just ["1"])
            (texts $ "a" // "b" `atDepth` 1)

    ,   scrapeTest
            "<a><b>1</b><c><b>2</b></c></a>"
            (Just ["2"])
            (texts $ "a" // "b" `atDepth` 2)

    ,   scrapeTest
            "<a><b class='foo'>1</b><c><b class='foo'>2</b></c></a>"
            (Just ["1"])
            (texts $ "a" // "b" @: [hasClass "foo"] `atDepth` 1)

    -- Depth should handle malformed HTML correctly. Below <b> and <c> are not
    -- closed in the proper order, but since <d> is nested within both in the
    -- context of <a>, <d> is still at depth 3.
    ,   scrapeTest
            "<a><b><c><d>1</d></b></c></a>"
            (Just ["1"])
            (texts $ "a" // "d" `atDepth` 3)

    -- However, from the context of <b>, <d> is only at depth 1 because there is
    -- no closing <c> tag within the <b> tag so the <c> tag is assumed to be
    -- self-closing.
    ,   scrapeTest
            "<a><b><c><d>2</d></b></c></a>"
            (Just ["2"])
            (texts $ "b" // "d" `atDepth` 1)

    ,   scrapeTest
            "<a><b><c><d>2</d></b></c></a>"
            (Just ["2"])
            (texts $ "b" // "d")

    ,   scrapeTest
            "<b><c><d>2</d></b></c>"
            (Just ["2"])
            (texts $ "b" // "d")

    ,   scrapeTest
            "<b><c><d>2</d></b></c>"
            (Just ["2"])
            (texts $ "c" // "d")

    ,   scrapeTest
            "1<a>2</a>3<b>4<c>5</c>6</b>7"
            (Just $ map show [1..7])
            (texts textSelector)

    ,   scrapeTest
            "1<a>2</a>3<b>4<c>5</c>6</b>7"
            (Just ["1", "2", "3", "456", "7"])
            (texts $ anySelector `atDepth` 0)

    ,   scrapeTest
            "<a><b><c><d>2</d></c></a></b>"
            (Just ["2"])
            (texts $ "a" // "d" `atDepth` 2)
    ]

scrapeTest :: (Eq a, Show a) => String -> Maybe a -> Scraper String a -> Test
scrapeTest html expected scraper = label ~: expected @=? actual
    where
        label  = "scrape (" ++ show html ++ ")"
        actual = scrape scraper (TagSoup.parseTags html)
