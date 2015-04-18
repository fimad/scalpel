{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Instances.ByteString.Lazy (
) where

import Text.HTML.Scalpel.Internal.Select.Types

import qualified Data.ByteString.Lazy as LBS


instance Selectable LBS.ByteString LBS.ByteString where
    toSelector node = MkSelector [SelectNode node []]

instance AttributeName LBS.ByteString LBS.ByteString where
    matchKey = (==)

instance TagName LBS.ByteString LBS.ByteString where
    toSelectNode = SelectNode
