{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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


(@:) :: (TagSoup.StringLike str, TagType str tag)
     => tag -> [AttributePredicate str] -> Selector str
(@:) tag attrs = [toSelectNode tag attrs]
infixl 9 @:

(@=) :: (TagSoup.StringLike str, AttributeKey str key)
     => key -> str -> AttributePredicate str
(@=) key value (attrKey, attrValue) =  matchKey key attrKey
                                    && value == attrValue
infixl 6 @=

(@=~) :: (TagSoup.StringLike str, AttributeKey str key, RE.RegexLike re str)
      => key -> re -> AttributePredicate str
(@=~) key re (attrKey, attrValue) =  matchKey key attrKey
                                  && RE.matchTest re attrValue
infixl 6 @=~

(//) :: (TagSoup.StringLike str, Selectable str a, Selectable str b)
    => a -> b -> Selector str
(//) a b = toSelector a ++ toSelector b
infixl 5 //

-- | TODO: Document me!
hasClass :: TagSoup.StringLike str => str -> AttributePredicate str
hasClass clazz (attrName, classes)
    | "class" == TagSoup.toString attrName = any (== textClass) classList
    | otherwise                            = False
    where textClass   = TagSoup.castString clazz
          textClasses = TagSoup.castString classes
          classList   = T.split (== ' ') textClasses
