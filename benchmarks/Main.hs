import Text.HTML.Scalpel

import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Criterion.Main (bgroup, bench, defaultMain, nf)
import Data.Foldable (foldr')
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TagSoup


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

makeNested :: Int -> [TagSoup.Tag T.Text]
makeNested i = TagSoup.parseTags
             $ T.concat [T.replicate i open, one, T.replicate i close]
    where
        open  = T.pack "<tag>"
        close = T.pack "</tag>"
        one   = T.pack "1"

sumListTags :: [TagSoup.Tag T.Text] -> Maybe Integer
sumListTags testData = flip scrape testData
                     $ sum <$> chroots "tag" (return 1)

manySelects :: Int -> [TagSoup.Tag T.Text] -> Maybe ()
manySelects i testData = flip scrape testData
                       $ replicateM_ i
                       $ sum <$> chroots "tag" (return 1)

manySelectNodes :: Int -> [TagSoup.Tag T.Text] -> Maybe T.Text
manySelectNodes i testData = flip scrape testData
                           $ text
                           $ foldr' (//) (toSelector "tag")
                           $ replicate (i - 1) (toSelector "tag")
