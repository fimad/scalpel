{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Text.HTML.Scalpel

import Control.Applicative
import System.Exit
import Test.HUnit

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.Regex.TDFA


main = exit . failures =<< runTestTT (TestList [
        scrapeTests
    ,   selectTests
    ])

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith $ ExitFailure n

selectTests = "selectTests" ~: TestList [
        selectTest
            ("a" @: [])
            "<a>foo</a>"
            ["<a>foo</a>"]

    ,   selectTest
            ("a" @: [])
            "<a>foo</a><a>bar</a>"
            ["<a>foo</a>", "<a>bar</a>"]

    ,   selectTest
            ("a" @: [])
            "<b><a>foo</a></b>"
            ["<a>foo</a>"]

    ,   selectTest
            ("a" @: [])
            "<a><a>foo</a></a>"
            ["<a><a>foo</a></a>", "<a>foo</a>"]

    ,   selectTest
            ("b" @: [])
            "<a>foo</a>"
            []

    ,   selectTest
            ("a" @: [])
            "<a>foo"
            []

    ,   selectTest
            ("a" @: ["key" @= "value"])
            "<a>foo</a><a key=value>bar</a>"
            ["<a key=value>bar</a>"]

    ,   selectTest
            ("a" // "b" @: [] // "c")
            "<a><b><c>foo</c></b></a>"
            ["<c>foo</c>"]

    ,   selectTest
            ("a" // "b")
            "<c><a><b>foo</b></a></c><c><a><d><b>bar</b></d></a></c><b>baz</b>"
            ["<b>foo</b>", "<b>bar</b>"]

    ,   selectTest
            ("a" @: [hasClass "a"])
            "<a class='a b'>foo</a>"
            ["<a class='a b'>foo</a>"]

    ,   selectTest
            ("a" @: [hasClass "c"])
            "<a class='a b'>foo</a>"
            []

    ,   selectTest
            ("a" @: ["key" @=~ re "va(foo|bar|lu)e"])
            "<a key=value>foo</a>"
            ["<a key=value>foo</a>"]

    ,   selectTest
            ("a" @: [Any @= "value"])
            "<a foo=value>foo</a><a bar=value>bar</a>"
            ["<a foo=value>foo</a>", "<a bar=value>bar</a>"]

    ,   selectTest
            ("a" @: [Any @= "value"])
            "<a foo=other>foo</a><a bar=value>bar</a>"
            ["<a bar=value>bar</a>"]

    ,   selectTest
            (Any @: [Any @= "value"])
            "<a foo=value>foo</a><b bar=value>bar</b>"
            ["<a foo=value>foo</a>", "<b bar=value>bar</b>"]

    ,   selectTest
            (Any @: [Any @= "value"])
            "<a foo=other>foo</a><b bar=value>bar</b>"
            ["<b bar=value>bar</b>"]
    ]

selectTest :: Selectable String s => s -> String -> [String] -> Test
selectTest selector tags expectedText = label ~: expected @=? actual
    where
        label  = "select (" ++ show tags ++ ")"
        expected = map TagSoup.parseTags expectedText
        actual = select selector (TagSoup.parseTags tags)

re :: String -> Text.Regex.TDFA.Regex
re = Text.Regex.TDFA.makeRegex

scrapeTests = "scrapeTests" ~: TestList [
        scrapeTest
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
            (map (== "foo") <$> (texts $ "a"))

    ,   scrapeTest
            "<a key=foo />"
            (Just "foo")
            (attr "key" $ "a")

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
            ((text $ "a" // "b") <|> (text $ "a" // "c"))

    ,   scrapeTest
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "bar")
            ((text $ "a" // "d") <|> (text $ "a" // "c"))
    ]

scrapeTest :: (Eq a, Show a) => String -> Maybe a -> Scraper String a -> Test
scrapeTest html expected scraper = label ~: expected @=? actual
    where
        label  = "scrape (" ++ show html ++ ")"
        actual = scrape scraper (TagSoup.parseTags html)
