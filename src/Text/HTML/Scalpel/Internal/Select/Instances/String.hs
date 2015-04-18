{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Instances.String (
) where

import Text.HTML.Scalpel.Internal.Select.Types


instance Selectable String String where
    toSelector node = MkSelector [SelectNode node []]

instance AttributeName String String where
    matchKey = (==)

instance TagName String String where
    toSelectNode = SelectNode
