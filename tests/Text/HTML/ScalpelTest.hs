{-# LANGUAGE FlexibleContexts #-}
module Text.HTML.ScalpelTest (tests) where

import Test.HUnit
import Text.HTML.Scalpel

import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TagSoup

tests = "Text.Html.Scalpel" ~: TestList [
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
    ]

selectTest :: (Show s, Selector s T.Text) => s -> T.Text -> [T.Text] -> Test
selectTest selector tags expectedText = label ~: expected @=? actual
    where
        label  = "test (" ++ show selector ++ ") on " ++ show tags
        expected = map TagSoup.parseTags expectedText
        actual = select selector (TagSoup.parseTags tags)
