module Text.HTML.Scalpel (
    Selector
,   Selectable (..)
,   AttributePredicate
,   AttributeName
,   TagName
,   Any (..)
,   (//)
,   (@:)
,   (@=)
,   (@=~)
,   hasClass
,   select

,   Scraper
,   attr
,   attrs
,   text
,   texts
,   chroot
,   chroots

,   scrape
,   scrapeURL
) where

import Text.HTML.Scalpel.Internal.Scrape
import Text.HTML.Scalpel.Internal.Scrape.URL
import Text.HTML.Scalpel.Internal.Select
import Text.HTML.Scalpel.Internal.Select.Types
import Text.HTML.Scalpel.Internal.Select.Combinators
