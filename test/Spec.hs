-- | Test-suite entry point: composes the per-module hspec specs into a
-- single run.
module Main (main) where

import qualified CI.JustfileSpec
import qualified CI.ProcessComposeSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  CI.JustfileSpec.spec
  CI.ProcessComposeSpec.spec
