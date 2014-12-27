{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.HTML.Scalpel (
    Selector
,   Selectable (..)
,   AttributePredicate
,   Any (..)
,   AttributeKey
,   TagType
,   (//)
,   (@:)
,   (@=)
,   (@=~)
,   hasClass
,   select

,   Scraper (..)
,   attr
,   attrs
,   text
,   texts
,   chroot
,   chroots
) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.Regex.Base.RegexLike as RE
import qualified Text.StringLike as TagSoup


--------------------------------------------------------------------------------
-- Selector definitions.

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

-- | TODO: Document me!
select :: (TagSoup.StringLike str, Selectable str s)
       => s -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
select s = selectNodes (toSelector s)

selectNodes :: TagSoup.StringLike str
            => [SelectNode str] -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
selectNodes nodes tags = head' $ reverse $ results
    where results = [concatMap (selectNode s) ts | s  <- nodes
                                                 | ts <- [tags] : results]
          head' []    = []
          head' (x:_) = x

selectNode :: TagSoup.StringLike str
           => SelectNode str -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
selectNode (SelectNode node attributes) tags = concatMap extractTagBlock nodes
    where nodes = filter (checkTag node attributes) $ tails tags
selectNode (SelectAny attributes) tags = concatMap extractTagBlock nodes
    where nodes = filter (checkPreds attributes) $ tails tags

-- Given a tag name and a list of attribute predicates return a function that
-- returns true if a given tag matches the supplied name and predicates.
checkTag :: TagSoup.StringLike str
          => str -> [AttributePredicate str] -> [TagSoup.Tag str] -> Bool
checkTag name preds tags@(TagSoup.TagOpen str attrs:_)
    = name == str && checkPreds preds tags
checkTag _ _ _ = False

checkPreds :: TagSoup.StringLike str
            => [AttributePredicate str] -> [TagSoup.Tag str] -> Bool
checkPreds preds (TagSoup.TagOpen str attrs:_)
    = and [or [p attr | attr <- attrs] | p <- preds]
checkPreds _ _ = False

-- Given a list of tags, return the prefix that of the tags up to the closing
-- tag that corresponds to the initial tag.
extractTagBlock :: TagSoup.StringLike str
                => [TagSoup.Tag str] -> [[TagSoup.Tag str]]
extractTagBlock [] = []
extractTagBlock (openTag : tags)
    | not $ TagSoup.isTagOpen openTag = []
    | otherwise                       = map (openTag :)
                                      $ splitBlock (getTagName openTag) 0 tags

splitBlock _    _       []                          = []
splitBlock name depth  (tag : tags)
    | depth == 0 && TagSoup.isTagCloseName name tag = [[tag]]
    | TagSoup.isTagCloseName name tag = tag_ $ splitBlock name (depth - 1) tags
    | TagSoup.isTagOpenName name tag  = tag_ $ splitBlock name (depth + 1) tags
    | otherwise                       = tag_ $ splitBlock name depth tags
    where tag_ = map (tag :)

getTagName :: TagSoup.StringLike str => TagSoup.Tag str -> str
getTagName (TagSoup.TagOpen name _) = name
getTagName (TagSoup.TagClose name)  = name
getTagName _                        = undefined

--------------------------------------------------------------------------------
-- Scraper definitions.

newtype Scraper str a = MkScraper {scrape :: [TagSoup.Tag str] -> Maybe a}

instance Functor (Scraper str) where
    fmap f (MkScraper a) = MkScraper $ fmap (fmap f) a

instance Applicative (Scraper str) where
    pure = MkScraper . const . Just
    (MkScraper f) <*> (MkScraper a) = MkScraper applied
        where applied tags | (Just aVal) <- a tags = ($ aVal) <$> f tags
                           | otherwise             = Nothing

instance Monad (Scraper str) where
    return = pure
    (MkScraper a) >>= f = MkScraper combined
        where combined tags | (Just aVal) <- a tags = let (MkScraper b) = f aVal
                                                      in  b tags
                            | otherwise             = Nothing

-- scrapeURL :: TagSoup.StringLike str => URL -> Scraper str a -> Maybe a
-- scrapeURL = error "TODO"

-- | TODO: Document this.
chroot :: (TagSoup.StringLike str, Selectable str s)
       => s -> Scraper str a -> Scraper str a
chroot selector (MkScraper inner) = MkScraper
                                  $ join . (inner <$>)
                                  . listToMaybe . select selector

-- | TODO: Document this.
chroots :: (TagSoup.StringLike str, Selectable str s)
        => s -> Scraper str a -> Scraper str [a]
chroots selector (MkScraper inner) = MkScraper
                                   $ return . catMaybes
                                   . map inner . select selector

-- | TODO: Document this.
text :: (TagSoup.StringLike str, Selectable str s) => s -> Scraper str str
text s = MkScraper $ withHead tagsToText . select s

-- | TODO: Document this.
texts :: (TagSoup.StringLike str, Selectable str s) => s -> Scraper str [str]
texts s = MkScraper $ withAll tagsToText . select s

-- | TODO: Document this.
attr :: (Show str, TagSoup.StringLike str, Selectable str s)
     => str -> s -> Scraper str str
attr name s = MkScraper $ join . withHead (tagsToAttr name) . select s

-- | TODO: Document this.
attrs :: (Show str, TagSoup.StringLike str, Selectable str s)
     => str -> s -> Scraper str [str]
attrs name s = MkScraper $ fmap catMaybes . withAll (tagsToAttr name) . select s

withHead :: (a -> b) -> [a] -> Maybe b
withHead _ []    = Nothing
withHead f (x:_) = Just $ f x

withAll :: (a -> b) -> [a] -> Maybe [b]
withAll _ [] = Nothing
withAll f xs = Just $ map f xs

tagsToText :: TagSoup.StringLike str => [TagSoup.Tag str] -> str
tagsToText = TagSoup.innerText

tagsToAttr :: (Show str, TagSoup.StringLike str)
           => str -> [TagSoup.Tag str] -> Maybe str
tagsToAttr attr tags = do
    tag <- listToMaybe tags
    guard $ TagSoup.isTagOpen tag
    return $ TagSoup.fromAttrib attr tag
