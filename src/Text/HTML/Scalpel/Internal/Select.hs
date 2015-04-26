{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select (
    select
) where

import Text.HTML.Scalpel.Internal.Select.Types

import Data.List

import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


-- | The 'select' function takes a 'Selectable' value and a list of
-- 'TagSoup.Tag's and returns a list of every subsequence of the given list of
-- Tags that matches the given selector.
select :: (TagSoup.StringLike str, Selectable s)
       => s -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
select s = selectNodes nodes
    where (MkSelector nodes) = toSelector s

selectNodes :: TagSoup.StringLike str
            => [SelectNode] -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
selectNodes nodes tags = head' $ reverse results
    where results = [concatMap (selectNode s) ts | s  <- nodes
                                                 | ts <- [tags] : results]
          head' []    = []
          head' (x:_) = x

selectNode :: TagSoup.StringLike str
           => SelectNode -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
selectNode (SelectNode node attributes) tags = concatMap extractTagBlock nodes
    where nodes = filter (checkTag node attributes) $ tails tags
selectNode (SelectAny attributes) tags = concatMap extractTagBlock nodes
    where nodes = filter (checkPreds attributes) $ tails tags

-- Given a tag name and a list of attribute predicates return a function that
-- returns true if a given tag matches the supplied name and predicates.
checkTag :: TagSoup.StringLike str
          => String -> [AttributePredicate] -> [TagSoup.Tag str] -> Bool
checkTag name preds tags@(TagSoup.TagOpen str _:_)
    = TagSoup.fromString name == str && checkPreds preds tags
checkTag _ _ _ = False

checkPreds :: TagSoup.StringLike str
            => [AttributePredicate] -> [TagSoup.Tag str] -> Bool
checkPreds preds (TagSoup.TagOpen _ attrs:_)
    = and [or [checkPred p attr | attr <- attrs] | p <- preds]
checkPreds _ _ = False

-- Given a list of tags, return the prefix of the tags up to the closing tag
-- that corresponds to the initial tag.
extractTagBlock :: TagSoup.StringLike str
                => [TagSoup.Tag str] -> [[TagSoup.Tag str]]
extractTagBlock [] = []
extractTagBlock (openTag : tags)
    | not $ TagSoup.isTagOpen openTag = []
    | otherwise                       = fakeClosingTag openTag
                                      $ map (openTag :)
                                      $ splitBlock (getTagName openTag) 0 tags
    where
        -- To handle tags that do not have a closing tag, fake an empty block by
        -- adding a closing tag.
        fakeClosingTag openTag@(TagSoup.TagOpen name _) []
            = [[openTag, TagSoup.TagClose name]]
        fakeClosingTag _ blocks = blocks

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
