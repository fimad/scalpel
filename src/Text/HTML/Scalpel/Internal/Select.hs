{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select (
    select
) where

import Text.HTML.Scalpel.Internal.Select.Types

import Control.Applicative ((<$>), (<|>))
import Control.Arrow (first)
import Data.List (tails)
import Data.Maybe (catMaybes)
import GHC.Exts (sortWith)

import qualified Data.Map.Strict as Map
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup


type CloseOffset = Maybe Int

-- | The 'select' function takes a 'Selectable' value and a list of
-- 'TagSoup.Tag's and returns a list of every subsequence of the given list of
-- Tags that matches the given selector.
select :: (Ord str, TagSoup.StringLike str, Selectable s)
       => s -> [TagSoup.Tag str] -> [[TagSoup.Tag str]]
select s = map (map fst) . selectNodes nodes . tagWithOffset
    where (MkSelector nodes) = toSelector s

-- | Annotate each tag with the offset to the corresponding closing tag. This
-- annotating is done in O(n * log(n)).
--
-- The algorithm works on a list of tags annotated with their index. It
-- maintains a map of unclosed open tags keyed by tag name.
--
--      (1) When an open tag is encountered it is pushed onto the list keyed by
--          its name.
--
--      (2) When a closing tag is encountered the corresponding opening tag is
--          popped, the offset between the two are computed, the opening tag is
--          annotated with the offset between the two, and both are added to the
--          result set.
--
--      (3) When any other tag is encountered it is added to the result set
--          immediately.
--
--      (4) After all tags are either in the result set or the state, all
--          unclosed tags from the state are added to the result set without a
--          closing offset.
--
--      (5) The result set is then sorted and the indices are stripped from the
--          tags.
tagWithOffset :: forall str. (Ord str, TagSoup.StringLike str)
              => [TagSoup.Tag str] -> [(TagSoup.Tag str, CloseOffset)]
tagWithOffset tags = let indexed  = zip tags [0..]
                         unsorted = go indexed Map.empty
                         sorted   = sortWith snd unsorted
                      in map fst sorted
    where
        go :: [(TagSoup.Tag str, Int)]
           -> Map.Map str [(TagSoup.Tag str, Int)]
           -> [((TagSoup.Tag str, CloseOffset), Int)]
        go [] state = map (first (, Nothing)) $ concat $ Map.elems state
        go (x@(tag, index) : xs) state
            | TagSoup.isTagClose tag =
                let maybeOpen = head <$> Map.lookup tagName state
                    state'    = Map.alter popTag tagName state
                    res       = catMaybes [
                                        Just ((tag, Nothing), index)
                                    ,   calcOffset <$> maybeOpen
                                    ]
                 in res ++ go xs state'
            | TagSoup.isTagOpen tag  = go xs (Map.alter appendTag tagName state)
            | otherwise              = ((tag, Nothing), index) : go xs state
            where
                tagName = getTagName tag

                appendTag :: Maybe [(TagSoup.Tag str, Int)]
                          -> Maybe [(TagSoup.Tag str, Int)]
                appendTag m = (x :) <$> (m <|> Just [])

                calcOffset :: (t, Int) -> ((t, Maybe Int), Int)
                calcOffset (t, i) =
                    let offset = index - i
                     in offset `seq` ((t, Just offset), i)

                popTag :: Maybe [a] -> Maybe [a]
                popTag (Just (_ : y : xs)) = let s = y : xs in s `seq` Just s
                popTag _                   = Nothing

selectNodes :: TagSoup.StringLike str
            => [SelectNode]
            -> [(TagSoup.Tag str, CloseOffset)]
            -> [[(TagSoup.Tag str, CloseOffset)]]
selectNodes nodes tags = head' $ reverse results
    where results = [concatMap (selectNode s) ts | s  <- nodes
                                                 | ts <- [tags] : results]
          head' []    = []
          head' (x:_) = x

selectNode :: TagSoup.StringLike str
           => SelectNode
           -> [(TagSoup.Tag str, CloseOffset)]
           -> [[(TagSoup.Tag str, CloseOffset)]]
selectNode (SelectNode node attributes) tags = concatMap extractTagBlock nodes
    where nodes = filter (checkTag node attributes) $ tails tags
selectNode (SelectAny attributes) tags = concatMap extractTagBlock nodes
    where nodes = filter (checkPreds attributes) $ tails tags

-- | Given a tag name and a list of attribute predicates return a function that
-- returns true if a given tag matches the supplied name and predicates.
checkTag :: TagSoup.StringLike str
          => String
          -> [AttributePredicate]
          -> [(TagSoup.Tag str, CloseOffset)]
          -> Bool
checkTag name preds tags@((TagSoup.TagOpen str _, _) : _)
    = TagSoup.fromString name == str && checkPreds preds tags
checkTag _ _ _ = False

checkPreds :: TagSoup.StringLike str
            => [AttributePredicate] -> [(TagSoup.Tag str, CloseOffset)] -> Bool
checkPreds preds ((TagSoup.TagOpen _ attrs, _) : _)
    = and [or [checkPred p attr | attr <- attrs] | p <- preds]
checkPreds _ _ = False

-- | Given a list of tags, return the prefix of the tags up to the closing tag
-- that corresponds to the initial tag.
extractTagBlock :: TagSoup.StringLike str
                => [(TagSoup.Tag str, CloseOffset)]
                -> [[(TagSoup.Tag str, CloseOffset)]]
extractTagBlock (ctag@(tag, maybeOffset) : tags)
    | not $ TagSoup.isTagOpen tag = []
    | Just offset <- maybeOffset  = [takeOrClose ctag offset tags]
    -- To handle tags that do not have a closing tag, fake an empty block by
    -- adding a closing tag. This function assumes that the tag is an open
    -- tag.
    | otherwise                   = [[ctag, (closeForOpen tag, Nothing)]]
extractTagBlock _                 = []

-- | Take offset number of elements from tags if available. If there are not
-- that many available, then fake a closing tag for the open tag. This happens
-- with malformed HTML that looks like `<a><b></a></b>`.
takeOrClose :: TagSoup.StringLike str
            => (TagSoup.Tag str, CloseOffset)
            -> Int
            -> [(TagSoup.Tag str, CloseOffset)]
            -> [(TagSoup.Tag str, CloseOffset)]
takeOrClose open@(tag, _) offset tags = go offset tags (open :)
    where
        go 0 _        f = f []
        go _ []       _ = [open, (closeForOpen tag, Nothing)]
        go i (x : xs) f = go (i - 1) xs (f . (x :))

closeForOpen :: TagSoup.StringLike str => TagSoup.Tag str -> TagSoup.Tag str
closeForOpen = TagSoup.TagClose . getTagName

getTagName :: TagSoup.StringLike str => TagSoup.Tag str -> str
getTagName (TagSoup.TagOpen name _) = name
getTagName (TagSoup.TagClose name)  = name
getTagName _                        = undefined
