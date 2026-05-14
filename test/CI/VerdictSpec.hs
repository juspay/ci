{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Verdict"'s pure verdict logic. The 'recordOutcome'
-- side of the module is exercised end-to-end by the smoke test in
-- @ci.just@; this spec covers 'runVerdictFrom' against handcrafted
-- outcome maps so the exit-code + summary-line contract is locked
-- down without spinning up process-compose.
module CI.VerdictSpec (spec) where

import CI.Gh (CommitStatus (..))
import CI.Verdict (runVerdictFrom)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Hspec

spec :: Spec
spec = describe "runVerdictFrom" $ do
  it "exits 0 and reports success when every recipe succeeded" $ do
    let (code, ls) = runVerdictFrom $ Map.fromList [("a", Success), ("b", Success)]
    code `shouldBe` ExitSuccess
    any ("a" `T.isInfixOf`) ls `shouldBe` True
    any ("b" `T.isInfixOf`) ls `shouldBe` True

  it "exits non-zero when any recipe is Failure" $ do
    let (code, _) = runVerdictFrom $ Map.fromList [("a", Success), ("b", Failure)]
    code `shouldBe` ExitFailure 1

  it "exits non-zero when any recipe is Error (skipped)" $ do
    let (code, _) = runVerdictFrom $ Map.fromList [("a", Success), ("b", Error)]
    code `shouldBe` ExitFailure 1

  it "exits non-zero when any recipe is still Pending (never reached terminal state)" $ do
    let (code, _) = runVerdictFrom $ Map.fromList [("a", Success), ("b", Pending)]
    code `shouldBe` ExitFailure 1

  it "lists every recipe in the summary lines" $ do
    let (_, ls) = runVerdictFrom $ Map.fromList [("alpha", Success), ("beta", Failure), ("gamma", Error)]
        joined = T.unlines ls
    "alpha" `T.isInfixOf` joined `shouldBe` True
    "beta" `T.isInfixOf` joined `shouldBe` True
    "gamma" `T.isInfixOf` joined `shouldBe` True

  it "renders the empty map as a successful no-op" $ do
    let (code, _) = runVerdictFrom Map.empty
    code `shouldBe` ExitSuccess
