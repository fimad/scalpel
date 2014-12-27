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

import Text.HTML.Scalpel.Internal.Scrape
import Text.HTML.Scalpel.Internal.Select
import Text.HTML.Scalpel.Internal.Select.Types
import Text.HTML.Scalpel.Internal.Select.Combinators
