{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.HTML.Scalpel.Internal.Select.Types (
    Selector
,   Selectable (..)
,   AttributePredicate
,   Any (..)
,   AttributeKey (..)
,   TagType (..)

,   SelectNode (..)
) where

import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TagSoup


class Selectable str s | s -> str where
    toSelector :: s -> Selector str

class AttributeKey str k | k -> str where
    matchKey :: k -> str -> Bool

class TagType str t | t -> str where
    toSelectNode :: t -> [AttributePredicate str] -> SelectNode str

type AttributePredicate str = TagSoup.Attribute str -> Bool

data Any str = Any

data SelectNode str = SelectNode str [AttributePredicate str]
                    | SelectAny [AttributePredicate str]

type Selector str = [SelectNode str]

instance Selectable str (Selector str) where
    toSelector = id

instance Selectable T.Text T.Text where
    toSelector node = [SelectNode node []]

instance Selectable String String where
    toSelector node = [SelectNode node []]

instance Selectable str (Any str) where
    toSelector = const [SelectAny []]

instance AttributeKey T.Text T.Text where
    matchKey = (==)

instance AttributeKey String String where
    matchKey = (==)

instance TagType String String where
    toSelectNode = SelectNode

instance TagType T.Text T.Text where
    toSelectNode = SelectNode

instance AttributeKey str (Any str) where
    matchKey = const . const True

instance TagType str (Any str) where
    toSelectNode = const SelectAny
