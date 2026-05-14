-- | Test-suite entry point: composes the per-module hspec specs into a
-- single run. Add new specs by importing them here and chaining their
-- 'Spec' values into 'main'.
module Main (main) where

import qualified CI.JustfileSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec CI.JustfileSpec.spec
