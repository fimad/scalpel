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
(@:) :: (TagSoup.StringLike str, TagName str tag)
     => tag -> [AttributePredicate str] -> Selector str
(@:) tag attrs = MkSelector [toSelectNode tag attrs]
infixl 9 @:

-- | The '@=' operator creates an 'AttributePredicate' that will match
-- attributes with the given name and value.
--
-- If you are attempting to match a specific class of a tag with potentially
-- multiple classes, you should use the 'hasClass' utility function.
(@=) :: (TagSoup.StringLike str, AttributeName str key)
     => key -> str -> AttributePredicate str
(@=) key value (attrKey, attrValue) =  matchKey key attrKey
                                    && value == attrValue
infixl 6 @=

-- | The '@=~' operator creates an 'AttributePredicate' that will match
-- attributes with the given name and whose value matches the given regular
-- expression.
(@=~) :: (TagSoup.StringLike str, AttributeName str key, RE.RegexLike re str)
      => key -> re -> AttributePredicate str
(@=~) key re (attrKey, attrValue) =  matchKey key attrKey
                                  && RE.matchTest re attrValue
infixl 6 @=~

-- | The '//' operator creates an 'Selector' by nesting one 'Selector' in
-- another. For example, @"div" // "a"@ will create a 'Selector' that matches
-- anchor tags that are nested arbitrarily deep within a div tag.
(//) :: (TagSoup.StringLike str, Selectable str a, Selectable str b)
    => a -> b -> Selector str
(//) a b = MkSelector (as ++ bs)
    where (MkSelector as) = toSelector a
          (MkSelector bs) = toSelector b
infixl 5 //

-- | The classes of a tag are defined in HTML as a space separated list given by
-- the @class@ attribute. The 'hasClass' function will match a @class@ attribute
-- if the given class appears anywhere in the space separated list of classes.
hasClass :: TagSoup.StringLike str => str -> AttributePredicate str
hasClass clazz (attrName, classes)
    | "class" == TagSoup.toString attrName = any (== textClass) classList
    | otherwise                            = False
    where textClass   = TagSoup.castString clazz
          textClasses = TagSoup.castString classes
          classList   = T.split (== ' ') textClasses
