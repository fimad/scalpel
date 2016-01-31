import Text.HTML.Scalpel

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
        ]

sumListTags :: T.Text -> Maybe Integer
sumListTags testData = scrapeStringLike testData
                     $ (sum . map (const 1)) <$> texts "tag"

