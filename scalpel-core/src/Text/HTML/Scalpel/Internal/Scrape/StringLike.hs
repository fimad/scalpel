{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.StringLike (
    scrapeStringLike
,   scrapeStringLikeT
) where

import Data.Functor.Identity
import qualified Text.HTML.Parser as HP
import Text.HTML.Scalpel.Internal.Scrape
import Data.Text (Text)

-- | The 'scrapeStringLike' function parses a 'StringLike' value into a list of
-- tags and executes a 'Scraper' on it.
scrapeStringLike :: Text -> Scraper a -> Maybe a
scrapeStringLike = fmap runIdentity . scrapeStringLikeT

-- | The 'scrapeStringLikeT' function parses a 'StringLike' value into a list of
-- tags and executes a 'ScraperT' on it. Since 'ScraperT' is a monad
-- transformer, the result is monadic.
scrapeStringLikeT :: (Monad m)
                  => Text -> ScraperT m a -> m (Maybe a)
scrapeStringLikeT tags scraper = scrapeT scraper
    (HP.parseTokens tags)
