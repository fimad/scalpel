import Text.HTML.Scalpel

import Control.Applicative ((<$>))
import Control.Monad (replicateM_)
import Weigh (mainWith, func)
import qualified Data.Text as T


main :: IO ()
main = mainWith $ do
    let nested100  = makeNested 100
    let nested1000 = makeNested 1000
    let nested10000 = makeNested 10000
    func "sum nested tags 100"   sumListTags nested100
    func "sum nested tags 1000"  sumListTags nested1000
    func "sum nested tags 10000" sumListTags nested10000
    func "select many tags 10"   (manySelects nested1000) 10
    func "select many tags 100"  (manySelects nested1000) 100
    func "select many tags 1000" (manySelects nested1000) 1000

makeNested :: Int -> T.Text
makeNested i = T.concat [T.replicate i open, one, T.replicate i close]
    where
        open  = T.pack "<tag>"
        close = T.pack "</tag>"
        one   = T.pack "1"

sumListTags :: T.Text -> Maybe Integer
sumListTags testData = scrapeStringLike testData
                     $ (sum . map (const 1)) <$> texts "tag"

manySelects :: T.Text -> Int -> Maybe ()
manySelects testData i = scrapeStringLike testData
                       $ replicateM_ i
                       $ texts "tag"
