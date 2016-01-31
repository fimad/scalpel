import Text.HTML.Scalpel

import Control.Monad (replicateM_)
import Criterion.Main (bgroup, bench, defaultMain, nf)
import qualified Data.Text as T
import qualified Data.Text.IO as T


main :: IO ()
main = do
    nested100  <- T.readFile "benchmarks/data/nested-100.html"
    nested1000 <- T.readFile "benchmarks/data/nested-1000.html"
    nested2000 <- T.readFile "benchmarks/data/nested-2000.html"
    defaultMain [
            bgroup "nested" [
                bench "100" $ nf sumListTags nested100
            ,   bench "1000" $ nf sumListTags nested1000
            ,   bench "2000" $ nf sumListTags nested2000
            ]
        ,   bgroup "many-selects" [
                bench "10"  $ nf (manySelects 10) nested1000
            ,   bench "100" $ nf (manySelects 100) nested1000
            ,   bench "1000" $ nf (manySelects 1000) nested1000
            ]
        ]

sumListTags :: T.Text -> Maybe Integer
sumListTags testData = scrapeStringLike testData
                     $ (sum . map (const 1)) <$> texts "tag"

manySelects :: Int -> T.Text -> Maybe ()
manySelects i testData = scrapeStringLike testData
                       $ replicateM_ i
                       $ texts "tag"
