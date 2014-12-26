module Main (main) where

import Test.HUnit
import System.Exit

import qualified Text.HTML.ScalpelTest as ScalpelTest


main = exit . failures =<< runTestTT (TestList [
        ScalpelTest.tests
    ])

exit :: Int -> IO ()
exit 0 = exitSuccess
exit n = exitWith $ ExitFailure n
