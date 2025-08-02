{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel.Core

import Control.Monad (replicateM_)
import Criterion.Main (bgroup, bench, defaultMain, nf, whnfIO)
import Data.Foldable (foldr')
import Criterion.Measurement
import Criterion.Measurement.Types (Measured(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.HTML.Parser as HP

main :: IO ()
main = do
    let nested100  = makeNested 100
    let nested1000 = makeNested 1000
    let nested10000 = makeNested 10000
    -- permalink: https://en.wikipedia.org/w/index.php?title=New_York_City&oldid=1292263955
    wikipediaArticle <- T.readFile "benchmarks/wikipedia-new-york-city.html"
    measureMemory wikipediaArticle
    defaultMain [
            bgroup "all h2 on 1.6 MiB Wikipedia article `New York City`" [
                bench "timings" (nf (scrapeStringLike wikipediaArticle) (texts "h2"))
              ]
        ,   bgroup "nested" [
                bench "100" $ nf sumListTags nested100
            ,   bench "1000" $ nf sumListTags nested1000
            ,   bench "10000" $ nf sumListTags nested10000
            ]
        ,   bgroup "many-selects" [
                bench "10" $ nf (manySelects 10) nested1000
            ,   bench "100" $ nf (manySelects 100) nested1000
            ,   bench "1000" $ nf (manySelects 1000) nested1000
            ]
        ,   bgroup "many-//" [
                bench "10" $ nf (manySelectNodes 10) nested1000
            ,   bench "100" $ nf (manySelectNodes 100) nested1000
            ,   bench "1000" $ nf (manySelectNodes 1000) nested1000
            ]
        ]

makeNested :: Int -> [HP.Token]
makeNested i = HP.parseTokens
             $ T.concat [T.replicate i open, one, T.replicate i close]
    where
        open  = T.pack "<tag>"
        close = T.pack "</tag>"
        one   = T.pack "1"

sumListTags :: [HP.Token] -> Maybe Integer
sumListTags testData = flip scrape testData
                     $ sum <$> chroots "tag" (return 1)

manySelects :: Int -> [HP.Token] -> Maybe ()
manySelects i testData = flip scrape testData
                       $ replicateM_ i
                       $ sum <$> chroots "tag" (return 1)

manySelectNodes :: Int -> [HP.Token] -> Maybe T.Text
manySelectNodes i testData = flip scrape testData
                           $ text
                           $ foldr' (//) (tagSelector "tag")
                           $ replicate (i - 1) (tagSelector "tag")

measureMemory :: T.Text -> IO ()
measureMemory t = do
    m <- measure (nf (scrapeStringLike t) (texts "h2")) 1
    let pma = (show . measPeakMbAllocated . fst) m
    putStrLn "running memory test"
    putStrLn "       scraping all h2 on 1.6 MiB Wikipedia article `New York City`"
    putStrLn $ "        peak memory allocated: " ++ pma ++ " MiB"
