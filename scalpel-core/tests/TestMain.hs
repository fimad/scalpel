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
            "htmls should extract matching tag"
            "<a>foo</a>"
            (Just ["<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "htmls should ignore non-matching tag"
            "<a>foo</a><a>bar</a>"
            (Just ["<a>foo</a>", "<a>bar</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "htmls should extract matching tag when it is nested"
            "<b><a>foo</a></b>"
            (Just ["<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "htmls should extract each matching tag even if it is nested"
            "<a><a>foo</a></a>"
            (Just ["<a><a>foo</a></a>", "<a>foo</a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "htmls with no matching nodes should result in an empty list"
            "<a>foo</a>"
            (Just [])
            (htmls ("b" @: []))

    ,   scrapeTest
            "unclosed tags should be treated as immediately closed"
            "<a>foo"
            (Just ["<a>"])
            (htmls ("a" @: []))

    ,   scrapeTest
            "scraping should obey attribute predicates"
            "<a>foo</a><a key=\"value\">bar</a>"
            (Just ["<a key=\"value\">bar</a>"])
            (htmls ("a" @: ["key" @= "value"]))

    ,   scrapeTest
            "selectors using // should match the deepest node"
            "<a><b><c>foo</c></b></a>"
            (Just ["<c>foo</c>"])
            (htmls ("a" // "b" @: [] // "c"))

    ,   scrapeTest
            "selectors using // should skip over irrelevant nodes"
            "<c><a><b>foo</b></a></c><c><a><d><b>bar</b></d></a></c><b>baz</b>"
            (Just ["<b>foo</b>", "<b>bar</b>"])
            (htmls ("a" // "b"))

    ,   scrapeTest
            "hasClass should match tags with multiple classes"
            "<a class=\"a b\">foo</a>"
            (Just ["<a class=\"a b\">foo</a>"])
            (htmls ("a" @: [hasClass "a"]))

    ,   scrapeTest
            "hasClass should not match tags without the specified class"
            "<a class=\"a b\">foo</a>"
            (Just [])
            (htmls ("a" @: [hasClass "c"]))

    , scrapeTest
            "notP should negate attribute predicates"
            "<a>foo</a><a class=\"a b\">bar</a><a class=\"b\">baz</a>"
            (Just ["foo", "baz"])
            (texts ("a" @: [notP $ hasClass "a"]))

    ,   scrapeTest
            "@=~ should match via regular expressions"
            "<a key=\"value\">foo</a>"
            (Just ["<a key=\"value\">foo</a>"])
            (htmls ("a" @: ["key" @=~ re "va(foo|bar|lu)e"]))

    ,   scrapeTest
            "AnyAttribute should match any attribute key"
            "<a foo=\"value\">foo</a><a bar=\"value\">bar</a>"
            (Just ["<a foo=\"value\">foo</a>", "<a bar=\"value\">bar</a>"])
            (htmls ("a" @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "AnyAttribute should not match any attribute value"
            "<a foo=\"other\">foo</a><a bar=\"value\">bar</a>"
            (Just ["<a bar=\"value\">bar</a>"])
            (htmls ("a" @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "AnyTag should match any tag with the corresponding attributes"
            "<a foo=\"value\">foo</a><b bar=\"value\">bar</b>"
            (Just ["<a foo=\"value\">foo</a>", "<b bar=\"value\">bar</b>"])
            (htmls (AnyTag @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "AnyTag should not match tags without the corresponding attributes"
            "<a foo=\"other\">foo</a><b bar=\"value\">bar</b>"
            (Just ["<b bar=\"value\">bar</b>"])
            (htmls (AnyTag @: [AnyAttribute @= "value"]))

    ,   scrapeTest
            "Custom predicates"
            "<a foo=\"bar\">1</a><a foo=\"foo\">2</a><a bar=\"bar\">3</a>"
            (Just ["<a foo=\"foo\">2</a>", "<a bar=\"bar\">3</a>"])
            (htmls (AnyTag @: [match (==)]))

    ,   scrapeTest
            "text should extract inner text from the first matching tag"
            "<a>foo</a>"
            (Just "foo")
            (text "a")

    ,   scrapeTest
            "text should extract inner text from only the first matching tag"
            "<a>foo</a><a>bar</a>"
            (Just "foo")
            (text "a")

    ,   scrapeTest
            "texts should extract inner text from all matching tags"
            "<a>foo</a><a>bar</a>"
            (Just ["foo", "bar"])
            (texts "a")

    ,   scrapeTest
            "fmap should work as expected"
            "<a>foo</a><a>bar</a>"
            (Just [True, False])
            (map (== "foo") <$> texts "a")

    ,   scrapeTest
            "attr extract matching attribute value"
            "<a key=foo />"
            (Just "foo")
            (attr "key" "a")

    ,   scrapeTest
            "attr extract matching attribute value with complex predicates"
            "<a key1=foo/><b key1=bar key2=foo /><a key1=bar key2=baz />"
            (Just "baz")
            (attr "key2" $ "a" @: ["key1" @= "bar"])

    ,   scrapeTest
            "chroot should limit context to just selected node"
            "<a><b>foo</b></a><b>bar</b>"
            (Just ["foo"])
            (chroot "a" $ texts "b")

    ,   scrapeTest
            "chroots should work for all matching nodes"
            "<a><b>foo</b></a><a><b>bar</b></a>"
            (Just ["foo", "bar"])
            (chroots "a" $ text "b")

    ,   scrapeTest
            "<|> should return first match if valid"
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "foo")
            (text ("a" // "b") <|> text ("a" // "c"))

    ,   scrapeTest
            "<|> should return second match if valid"
            "<a><b>foo</b></a><a><c>bar</c></a>"
            (Just "bar")
            (text ("a" // "d") <|> text ("a" // "c"))

    ,   scrapeTest
            "Unclosed tags should be treated as immediately closed"
            "<img src='foobar'>"
            (Just "foobar")
            (attr "src" "img")

    ,   scrapeTest
            "scraping should work for self-closing tags"
            "<img src='foobar' />"
            (Just "foobar")
            (attr "src" "img")

    ,   scrapeTest
            "lower case selectors should match any case tag"
            "<a>foo</a><A>bar</A>"
            (Just ["foo", "bar"])
            (texts "a")

    ,   scrapeTest
            "upper case selectors should match any case tag"
            "<a>foo</a><A>bar</A>"
            (Just ["foo", "bar"])
            (texts "A")

    ,   scrapeTest
            "attribute key matching should be case-insensitive"
            "<a B=C>foo</a>"
            (Just ["foo"])
            (texts $ "A" @: ["b" @= "C"])

    ,   scrapeTest
            "attribute value matching should be case-sensitive"
            "<a B=C>foo</a>"
            (Just [])
            (texts $ "A" @: ["b" @= "c"])

    , scrapeTest
            "notP should invert attribute value matching"
            "<a>foo</a><a B=C>bar</a><a B=D>baz</a>"
            (Just ["foo", "baz"])
            (texts ("a" @: [notP $ "b" @= "C"]))

    ,   scrapeTest
            "html should work when matching the root node"
            "<a>foo</a>"
            (Just "<a>foo</a>")
            (html "a")

    ,   scrapeTest
            "html should work when matching a nested node"
            "<body><div><ul><li>1</li><li>2</li></ul></div></body>"
            (Just "<li>1</li>")
            (html "li")

    ,   scrapeTest
            "html should work when matching a node with no inner text"
            "<body><div></div></body>"
            (Just "<div></div>")
            (html "div")

    ,   scrapeTest
            "htmls should return html matching root nodes"
            "<a>foo</a><a>bar</a>"
            (Just ["<a>foo</a>","<a>bar</a>"])
            (htmls "a")

    ,   scrapeTest
            "htmls should return html matching nested nodes"
            "<body><div><ul><li>1</li><li>2</li></ul></div></body>"
            (Just ["<li>1</li>", "<li>2</li>"])
            (htmls "li")

    ,   scrapeTest
            "htmls should return html matching empty nested nodes"
            "<body><div></div></body>"
            (Just ["<div></div>"])
            (htmls "div")

    ,   scrapeTest
            "innerHTML should exclude root tags"
            "<a>1<b>2</b>3</a>"
            (Just "1<b>2</b>3")
            (innerHTML anySelector)

    ,   scrapeTest
            "innerHTML of a self closed tag should be the empty string"
            "<a>"
            (Just "")
            (innerHTML anySelector)

    ,   scrapeTest
            "innerHTML should match root nodes"
            "<a>foo</a><a>bar</a>"
            (Just ["foo","bar"])
            (innerHTMLs "a")

    ,   scrapeTest
            "guard should stop matches"
            "<a>foo</a><a>bar</a><a>baz</a>"
            (Just "<a>bar</a>")
            (chroot "a" $ do
                t <- text anySelector
                guard ("b" `isInfixOf` t)
                html anySelector)

    ,   scrapeTest
            "// should force a decent before matching"
            "<div id=\"outer\"><div id=\"inner\">inner text</div></div>"
            (Just ["inner"])
            (attrs "id" ("div" // "div"))

    ,   scrapeTest
            "div // div should match div/div/div twice"
            "<div id=\"a\"><div id=\"b\"><div id=\"c\"></div></div></div>"
            (Just ["b", "c"])
            (attrs "id" ("div" // "div"))

    ,   scrapeTest
            "anySelector should match the root node"
            "<a>1<b>2<c>3</c>4</b>5</a>"
            (Just "12345")
            (text anySelector)

    ,   scrapeTest
            "failing a pattern match should stop a scraper"
            "<a>1</a>"
            Nothing
            $ do
                "Bad pattern" <- text "a"
                return "OK"

    ,   scrapeTest
            "passing a pattern match should not stop a scraper"
            "<a>1</a>"
            (Just "OK")
            $ do
                "1" <- text "a"
                return "OK"
    ,   scrapeTest
            "position should return the index of the match"
            "<article><p>A</p><p>B</p><p>C</p></article>"
            (Just [(0, "A"), (1, "B"), (2, "C")])
            (chroots ("article" // "p") $ do
                index   <- position
                content <- text anySelector
                return (index, content))

    ,   scrapeTest
            "position should return the index of most recent match"
            "<article><p>A</p></article><article><p>B</p><p>C</p></article>"
            (Just [[(0, "A")], [(0, "B"), (1, "C")]])
            (chroots "article" $ chroots "p" $ do
                index   <- position
                content <- text anySelector
                return (index, content))

    ,   scrapeTest
            "DFS regression test for #59 (1)"
            "<div><p>p1</p><p>p2</p><blockquote><p>p3</p></blockquote><p>p4</p>"
            (Just ["p1", "p2", "p3", "p4"])
            (texts "p")

    ,   scrapeTest
            "DFS regression test for #59 (2)"
            "<a><b>1</b></a><a><b>2</b></a><a><b>3</b></a>"
            (Just ["1","2","3"])
            (texts "a")

    ,   scrapeTest
            "DFS regression test for #59 (3)"
            "<a><b>1</b></a><a><b>2</b></a><a><b>3</b></a>"
            (Just ["1","2","3"])
            (texts $ "a" // "b")

    ,   scrapeTest
            "DFS regression test for #59 (4)"
            "<a><b>1</b></a><a><b>2</b></a><a><b>3</b></a>"
            (Just ["1","2","3"])
            (texts "b")

    ,   scrapeTest
            "atDepth 1 should select immediate children"
            "<a><b>1</b><c><b>2</b></c></a>"
            (Just ["1"])
            (texts $ "a" // "b" `atDepth` 1)

    ,   scrapeTest
            "atDepth 2 should select children children"
            "<a><b>1</b><c><b>2</b></c></a>"
            (Just ["2"])
            (texts $ "a" // "b" `atDepth` 2)

    ,   scrapeTest
            "atDepth should compose with attribute predicates"
            "<a><b class='foo'>1</b><c><b class='foo'>2</b></c></a>"
            (Just ["1"])
            (texts $ "a" // "b" @: [hasClass "foo"] `atDepth` 1)

    -- Depth should handle malformed HTML correctly. Below <b> and <c> are not
    -- closed in the proper order, but since <d> is nested within both in the
    -- context of <a>, <d> is still at depth 3.
    ,   scrapeTest
            "atDepth should handle tags closed out of order (full context)"
            "<a><b><c><d>1</d></b></c></a>"
            (Just ["1"])
            (texts $ "a" // "d" `atDepth` 3)

    -- However, from the context of <b>, <d> is only at depth 1 because there is
    -- no closing <c> tag within the <b> tag so the <c> tag is assumed to be
    -- self-closing.
    ,   scrapeTest
            "atDepth should handle tags closed out of order (partial context)"
            "<a><b><c><d>2</d></b></c></a>"
            (Just ["2"])
            (texts $ "b" // "d" `atDepth` 1)

    ,   scrapeTest
            "// should handle tags closed out of order"
            "<a><b><c><d>2</d></b></c></a>"
            (Just ["2"])
            (texts $ "b" // "d")

    ,   scrapeTest
            "// should handle tags closed out of order for the root (1)"
            "<b><c><d>2</d></b></c>"
            (Just ["2"])
            (texts $ "b" // "d")

    ,   scrapeTest
            "// should handle tags closed out of order for the root (2)"
            "<b><c><d>2</d></b></c>"
            (Just ["2"])
            (texts $ "c" // "d")

    ,   scrapeTest
            "textSelector should select each text node"
            "1<a>2</a>3<b>4<c>5</c>6</b>7"
            (Just $ map show [1..7])
            (texts textSelector)

    ,   scrapeTest
            "anySelector should select text nodes"
            "1<a>2</a>3<b>4<c>5</c>6</b>7"
            (Just ["1", "2", "3", "456", "7"])
            (texts $ anySelector `atDepth` 0)

    ,   scrapeTest
            "atDepth should treat out of focus close tags as immediately closed"
            "<a><b><c><d>2</d></c></a></b>"
            (Just ["2"])
            (texts $ "a" // "d" `atDepth` 2)

    ,   scrapeTest
            "Applicative sanity checks for SerialScraper"
            "<a>1</a><b>2</b><a>3</a>"
            (Just ("1", "2"))
            (inSerial $ (,) <$> stepNext (text "a")
                            <*> stepNext (text "b"))

    ,   scrapeTest
            "Monad sanity checks for SerialScraper"
            "<a>1</a><b>2</b><a>3</a>"
            (Just ("1", "2"))
            (inSerial $ do
              a <- stepNext (text "a")
              b <- stepNext (text "b")
              return (a, b))

    ,   scrapeTest
            "stepping off the end of the list without reading should be allowed"
            "<a>1</a><b>2</b><a>3</a>"
            (Just ["1", "2", "3", "2" , "1"])
            (inSerial $ do
              a <- stepNext $ text anySelector
              b <- stepNext $ text anySelector
              c <- stepNext $ text anySelector
              d <- stepBack $ text anySelector
              e <- stepBack $ text anySelector
              return [a, b, c, d, e])

    ,   scrapeTest
            "stepping off the end of the list and reading should fail"
            "<a>1</a><b>2</b><a>3</a>"
            Nothing
            (inSerial $ (,,,) <$> stepNext (text anySelector)
                              <*> stepNext (text anySelector)
                              <*> stepNext (text anySelector)
                              <*> stepNext (text anySelector))

    ,   scrapeTest
            "seeking should skip over nodes"
            "<a>1</a><b>2</b><a>3</a>"
            (Just ("2", "3"))
            (inSerial $ (,) <$> seekNext (text "b")
                            <*> seekNext (text "a"))

    ,   scrapeTest
            "seeking should fail if there is not matching node"
            "<a>1</a><b>2</b><a>3</a>"
            Nothing
            (inSerial $ seekNext $ text "c")

    ,   scrapeTest
            "seeking off the end the zipper should be allowed without reading"
            "<a>1</a><b>2</b><c>3</c>"
            (Just ("3", "1"))
            (inSerial $ (,) <$> seekNext (text "c")
                            <*> seekBack (text "a"))

    ,   scrapeTest
            "Alternative sanity check for SerialScraper"
            "1<a foo=bar>2</a>3"
            (Just ["1", "bar", "3"])
            (inSerial $   many
                      $   stepNext (text $ textSelector `atDepth` 0)
                      <|> stepNext (attr "foo" $ "a" `atDepth` 0))

    ,   scrapeTest
            "MonadFail sanity check for SerialScraper (passing check)"
            "1"
            (Just "OK")
            (inSerial $ do
              "1" <- stepNext $ text textSelector
              return "OK")

    ,   scrapeTest
            "MonadFail sanity check for SerialScraper (failing check)"
            "1"
            Nothing
            (inSerial $ do
              "mismatch" <- stepNext $ text textSelector
              return "OK")

    ,   scrapeTest
            "untilNext should stop at first match"
            "1<a>2</a><b>3</b>"
            (Just ["1", "2"])
            (inSerial $ untilNext (matches "b")
                      $ many $ stepNext $ text anySelector)

    ,   scrapeTest
            "untilNext should go till end of the zipper on no match"
            "1<a>2</a><b>3</b>"
            (Just ["1", "2", "3"])
            (inSerial $ untilNext (matches "c")
                      $ many $ stepNext $ text anySelector)

    ,   scrapeTest
            "untilNext should leave the focus at the match"
            "1<a>2</a><b>3</b>"
            (Just "3")
            (inSerial $ do
              untilNext (matches "b") $ many $ stepNext $ text anySelector
              stepNext $ text "b")

    ,   scrapeTest
            "untilNext should create valid a empty context"
            "<a>1</a><a>2</a>"
            (Just "1")
            (inSerial $ do
              untilNext (matches "a") $ return ()
              stepNext $ text "a")

    ,   scrapeTest
            "scraping within an empty context should fail"
            "<a>1</a><a>2</a>"
            Nothing
            (inSerial $ do
              untilNext (matches "a") $ stepNext $ text anySelector
              stepNext $ text "a")

    ,   scrapeTest
            "untilBack should leave the focus of the new context at the end"
            "<b foo=bar /><a>1</a><a>2</a><a>3</a>"
            (Just ("bar", ["1", "2", "3"], ["2", "1"]))
            (inSerial $ do
              as <- many $ seekNext $ text "a"
              as' <- untilBack (matches "b") $ many $ stepBack $ text "a"
              b <- stepBack $ attr "foo" "b"
              return (b, as, as'))

    ,   scrapeTest
            "inSerial in a chroot should visit immediate children"
            "<parent><a>1</a><b>2</b></parent>"
            (Just ["1", "2"])
            (chroot "parent" $ inSerial $
              many $ stepNext $ text anySelector)

    ,   scrapeTest
            "Issue #41 regression test"
            "<p class='something'>Here</p><p>Other stuff that matters</p>"
            (Just "Other stuff that matters")
            (inSerial $ do
              seekNext $ matches $ "p" @: [hasClass "something"]
              stepNext $ text "p")

    ,   scrapeTest
            "Issue #45 regression test"
            (unlines [
              "<body>"
            , "  <h1>title1</h1>"
            , "  <h2>title2 1</h2>"
            , "  <p>text 1</p>"
            , "  <p>text 2</p>"
            , "  <h2>title2 2</h2>"
            , "  <p>text 3</p>"
            , "  <h2>title2 3</h2>"
            , "</body>"
            ])
            (Just [
              ("title2 1", ["text 1", "text 2"])
            , ("title2 2", ["text 3"])
            , ("title2 3", [])
            ])
            (chroot "body" $ inSerial $ many $ do
                title <- seekNext $ text "h2"
                ps <- untilNext (matches "h2") (many $ do
                  -- New lines between tags count as text nodes, skip over
                  -- these.
                  optional $ stepNext $ matches textSelector
                  stepNext $ text "p")
                return (title, ps))
    ]

scrapeTest :: (Eq a, Show a)
           => String -> String -> Maybe a -> Scraper String a -> Test
scrapeTest label html expected scraper = label ~: expected @=? actual
    where
        label' = label ++ ": scrape (" ++ show html ++ ")"
        actual = scrape scraper (TagSoup.parseTags html)
