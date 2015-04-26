{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Combinators (
    (//)
,   (@:)
,   (@=)
,   (@=~)
,   hasClass
) where

import Text.HTML.Scalpel.Internal.Select.Types

import qualified Data.Text as T
import qualified Text.Regex.Base.RegexLike as RE
import qualified Text.StringLike as TagSoup


-- | The '@:' operator creates a 'Selector' by combining a 'TagName' with a list
-- of 'AttributePredicate's.
(@:) :: TagName tag => tag -> [AttributePredicate] -> Selector
(@:) tag attrs = MkSelector [toSelectNode tag attrs]
infixl 9 @:

-- | The '@=' operator creates an 'AttributePredicate' that will match
-- attributes with the given name and value.
--
-- If you are attempting to match a specific class of a tag with potentially
-- multiple classes, you should use the 'hasClass' utility function.
(@=) :: AttributeName key => key -> String -> AttributePredicate
(@=) key value = MkAttributePredicate $ \(attrKey, attrValue) ->
                                         matchKey key attrKey
                                      && TagSoup.fromString value == attrValue
infixl 6 @=

-- | The '@=~' operator creates an 'AttributePredicate' that will match
-- attributes with the given name and whose value matches the given regular
-- expression.
(@=~) :: (AttributeName key, RE.RegexLike re String)
      => key -> re -> AttributePredicate
(@=~) key re = MkAttributePredicate $ \(attrKey, attrValue) ->
       matchKey key attrKey
    && RE.matchTest re (TagSoup.toString attrValue)
infixl 6 @=~

-- | The '//' operator creates an 'Selector' by nesting one 'Selector' in
-- another. For example, @"div" // "a"@ will create a 'Selector' that matches
-- anchor tags that are nested arbitrarily deep within a div tag.
(//) :: (Selectable a, Selectable b) => a -> b -> Selector
(//) a b = MkSelector (as ++ bs)
    where (MkSelector as) = toSelector a
          (MkSelector bs) = toSelector b
infixl 5 //

-- | The classes of a tag are defined in HTML as a space separated list given by
-- the @class@ attribute. The 'hasClass' function will match a @class@ attribute
-- if the given class appears anywhere in the space separated list of classes.
hasClass :: String -> AttributePredicate
hasClass clazz = MkAttributePredicate hasClass'
    where
        hasClass' (attrName, classes)
            | "class" == TagSoup.toString attrName = textClass `elem` classList
            | otherwise                            = False
            where textClass   = TagSoup.castString clazz
                  textClasses = TagSoup.castString classes
                  classList   = T.split (== ' ') textClasses
