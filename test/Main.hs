module Main where

import Test.DocTest
import System.FilePath.Glob (glob)

main :: IO ()
main = glob "src/*/*.hs" >>= doctest
