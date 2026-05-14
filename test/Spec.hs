module Main (main) where

import qualified CI.JustfileSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec CI.JustfileSpec.spec
