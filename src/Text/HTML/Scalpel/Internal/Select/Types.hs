{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Types (
    Selector (..)
,   Selectable (..)
,   AttributePredicate
,   Any (..)
,   AttributeName (..)
,   TagName (..)

,   SelectNode (..)
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Text.HTML.TagSoup as TagSoup


-- | The 'Selectable' class defines a class of types that are capable of being
-- cast into a 'Selector' which in turns describes a section of an HTML DOM
-- tree.
class Selectable str s | s -> str where
    toSelector :: s -> Selector str

-- | The 'AttributeName' class defines a class of types that can be used when
-- creating 'Selector's to specify the name of an attribute of a tag.
--
-- The most basic types of 'AttributeName' are the string like types (e.g.
-- 'String', 'T.Text', etc). Values of these types refer to attributes with
-- names of that value.
--
-- In addition there is also the 'Any' type which will match any attribute name.
class AttributeName str k | k -> str where
    matchKey :: k -> str -> Bool

-- | The 'TagName' class defines a class of types that can be used when creating
-- 'Selector's to specify the name of a tag.
--
-- The most basic types of 'TagName' are the string like types (e.g.  'String',
-- 'T.Text', etc). Values of these types refer to tags of the given value.
--
-- In addition there is also the 'Any' type which will match any tag.
class TagName str t | t -> str where
    toSelectNode :: t -> [AttributePredicate str] -> SelectNode str

-- | An 'AttributePredicate' is a method that takes a 'TagSoup.Attribute' and
-- returns a 'Bool' indicating if the given attribute matches a predicate.
type AttributePredicate str = TagSoup.Attribute str -> Bool

-- | 'Any' can be used as a wildcard when constructing selectors to match tags
-- and attributes with any name.
--
-- For example, the selector @Any \@: [Any \@= \"foo\"]@ matches all tags that
-- have any attribute where the value is @\"foo\"@.
data Any str = Any

-- | 'Selector' defines a selection of an HTML DOM tree to be operated on by
-- a web scraper. The selection includes the opening tag that matches the
-- selection, all of the inner tags, and the corresponding closing tag.
newtype Selector str = MkSelector [SelectNode str]

data SelectNode str = SelectNode str [AttributePredicate str]
                    | SelectAny [AttributePredicate str]

instance Selectable str (Selector str) where
    toSelector = id

instance Selectable String String where
    toSelector node = MkSelector [SelectNode node []]

instance Selectable BS.ByteString BS.ByteString where
    toSelector node = MkSelector [SelectNode node []]

instance Selectable LBS.ByteString LBS.ByteString where
    toSelector node = MkSelector [SelectNode node []]

instance Selectable T.Text T.Text where
    toSelector node = MkSelector [SelectNode node []]

instance Selectable LT.Text LT.Text where
    toSelector node = MkSelector [SelectNode node []]

instance Selectable str (Any str) where
    toSelector = const (MkSelector [SelectAny []])

instance AttributeName str (Any str) where
    matchKey = const . const True

instance AttributeName String String where
    matchKey = (==)

instance AttributeName BS.ByteString BS.ByteString where
    matchKey = (==)

instance AttributeName LBS.ByteString LBS.ByteString where
    matchKey = (==)

instance AttributeName T.Text T.Text where
    matchKey = (==)

instance AttributeName LT.Text LT.Text where
    matchKey = (==)

instance TagName str (Any str) where
    toSelectNode = const SelectAny

instance TagName String String where
    toSelectNode = SelectNode

instance TagName BS.ByteString BS.ByteString where
    toSelectNode = SelectNode

instance TagName LBS.ByteString LBS.ByteString where
    toSelectNode = SelectNode

instance TagName T.Text T.Text where
    toSelectNode = SelectNode

instance TagName LT.Text LT.Text where
    toSelectNode = SelectNode
