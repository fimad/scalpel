import Text.HTML.Scalpel

import Control.Monad (replicateM_)
import Criterion.Main (bgroup, bench, defaultMain, nf)
import qualified Data.Text as T


main :: IO ()
main = do
    let nested100  = makeNested 100
    let nested1000 = makeNested 1000
    let nested10000 = makeNested 10000
    defaultMain [
            bgroup "nested" [
                bench "100" $ nf sumListTags nested100
            ,   bench "1000" $ nf sumListTags nested1000
            ,   bench "10000" $ nf sumListTags nested10000
            ]
        ,   bgroup "many-selects" [
                bench "10"  $ nf (manySelects 10) nested1000
            ,   bench "100" $ nf (manySelects 100) nested1000
            ,   bench "1000" $ nf (manySelects 1000) nested1000
            ]
        ]

makeNested :: Int -> T.Text
makeNested i = T.concat [T.replicate i open, one, T.replicate i close]
    where
        open  = T.pack "<tag>"
        close = T.pack "</tag>"
        one   = T.pack "1"

sumListTags :: T.Text -> Maybe Integer
sumListTags testData = scrapeStringLike testData
                     $ (sum . map (const 1)) <$> texts "tag"

manySelects :: Int -> T.Text -> Maybe ()
manySelects i testData = scrapeStringLike testData
                       $ replicateM_ i
                       $ texts "tag"
