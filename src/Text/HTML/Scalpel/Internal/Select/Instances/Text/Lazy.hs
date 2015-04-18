{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Instances.Text.Lazy (
) where

import Text.HTML.Scalpel.Internal.Select.Types

import qualified Data.Text.Lazy as LT


instance Selectable LT.Text LT.Text where
    toSelector node = MkSelector [SelectNode node []]

instance AttributeName LT.Text LT.Text where
    matchKey = (==)

instance TagName LT.Text LT.Text where
    toSelectNode = SelectNode
