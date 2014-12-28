module Text.HTML.Scalpel (
-- * Selectors
    Selector
,   Selectable (..)
,   AttributePredicate
,   AttributeName
,   TagName
,   Any (..)

-- ** Tag combinators
,   (//)

-- ** Attribute combinators
,   (@:)
,   (@=)
,   (@=~)
,   hasClass

-- ** Executing selectors
,   select

-- * Scrapers
,   Scraper
-- ** Combinators
,   attr
,   attrs
,   text
,   texts
,   chroot
,   chroots

-- ** Executing scrapers
,   scrape
,   scrapeStringLike
,   URL
,   scrapeURL
) where

import Text.HTML.Scalpel.Internal.Scrape
import Text.HTML.Scalpel.Internal.Scrape.StringLike
import Text.HTML.Scalpel.Internal.Scrape.URL
import Text.HTML.Scalpel.Internal.Select
import Text.HTML.Scalpel.Internal.Select.Combinators
import Text.HTML.Scalpel.Internal.Select.Types
