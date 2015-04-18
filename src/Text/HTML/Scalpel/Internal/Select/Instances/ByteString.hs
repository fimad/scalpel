{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Select.Instances.ByteString (
) where

import Text.HTML.Scalpel.Internal.Select.Types

import qualified Data.ByteString as BS


instance Selectable BS.ByteString BS.ByteString where
    toSelector node = MkSelector [SelectNode node []]

instance AttributeName BS.ByteString BS.ByteString where
    matchKey = (==)

instance TagName BS.ByteString BS.ByteString where
    toSelectNode = SelectNode
