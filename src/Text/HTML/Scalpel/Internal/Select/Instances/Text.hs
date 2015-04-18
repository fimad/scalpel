{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Instances.Text (
) where

import Text.HTML.Scalpel.Internal.Select.Types

import qualified Data.Text as T


instance Selectable T.Text T.Text where
    toSelector node = MkSelector [SelectNode node []]

instance AttributeName T.Text T.Text where
    matchKey = (==)

instance TagName T.Text T.Text where
    toSelectNode = SelectNode
