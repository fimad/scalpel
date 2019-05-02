{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Text.HTML.Scalpel.Internal.Select.Types (
    Selector (..)
,   AttributePredicate (..)
,   checkPred
,   AttributeName (..)
,   matchKey
,   anyAttrPredicate
,   TagName (..)
,   SelectNode (..)
,   tagSelector
,   anySelector
,   textSelector
,   toSelectNode
,   SelectSettings (..)
,   defaultSelectSettings
) where

import Data.Char (toLower)
import Data.String (IsString, fromString)
import Text.StringLike (StringLike, strMap, castString)

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup
import qualified Data.Text as T


-- | The 'AttributeName' type can be used when creating 'Selector's to specify
-- the name of an attribute of a tag.
data StringLike str => AttributeName str = AnyAttribute | AttributeString str

matchKey :: StringLike str => AttributeName str -> str -> Bool
matchKey (AttributeString s) = ((strMap toLower s) ==)
matchKey AnyAttribute = const True

instance StringLike str => IsString (AttributeName str) where
    fromString = AttributeString . castString

-- | An 'AttributePredicate' is a method that takes a 'TagSoup.Attribute' and
-- returns a 'Bool' indicating if the given attribute matches a predicate.
data AttributePredicate
        = MkAttributePredicate
                (forall str. StringLike str => [TagSoup.Attribute str]
                                                    -> Bool)

checkPred :: StringLike str
          => AttributePredicate -> [TagSoup.Attribute str] -> Bool
checkPred (MkAttributePredicate p) = p

-- | Creates an 'AttributePredicate' from a predicate function of a single
-- attribute that matches if any one of the attributes matches the predicate.
anyAttrPredicate :: (forall str. StringLike str => (str, str) -> Bool)
                 -> AttributePredicate
anyAttrPredicate p = MkAttributePredicate $ any p

-- | 'Selector' defines a selection of an HTML DOM tree to be operated on by
-- a web scraper. The selection includes the opening tag that matches the
-- selection, all of the inner tags, and the corresponding closing tag.
newtype Selector = MkSelector [(SelectNode, SelectSettings)]

-- | 'SelectSettings' defines additional criteria for a Selector that must be
-- satisfied in addition to the SelectNode. This includes criteria that are
-- dependent on the context of the current node, for example the depth in
-- relation to the previously matched SelectNode.
data SelectSettings = SelectSettings {
  -- | The required depth of the current select node in relation to the
  -- previously matched SelectNode.
  selectSettingsDepth :: Maybe Int
}

defaultSelectSettings :: SelectSettings
defaultSelectSettings = SelectSettings {
  selectSettingsDepth = Nothing
}

tagSelector :: StringLike str => str -> Selector
tagSelector tag = MkSelector [
    (toSelectNode (TagString tag) [], defaultSelectSettings)
  ]

-- | A selector which will match any node (including tags and bare text).
anySelector :: Selector
anySelector = MkSelector [(SelectAny [], defaultSelectSettings)]

-- | A selector which will match all text nodes.
textSelector :: Selector
textSelector = MkSelector [(SelectText, defaultSelectSettings)]

instance IsString Selector where
  fromString = tagSelector

data SelectNode = SelectNode !T.Text [AttributePredicate]
                | SelectAny [AttributePredicate]
                | SelectText

-- | The 'TagName' type is used when creating a 'Selector' to specify the name
-- of a tag.
data StringLike str => TagName str = AnyTag | TagString str

instance StringLike str => IsString (TagName str) where
    fromString = TagString . castString

toSelectNode :: StringLike str => TagName str -> [AttributePredicate] -> SelectNode
toSelectNode AnyTag = SelectAny
toSelectNode (TagString str) = SelectNode $ T.map toLower $ castString str
