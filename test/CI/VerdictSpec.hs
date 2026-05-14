{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Verdict"'s pure verdict logic. The 'recordOutcome'
-- side of the module is exercised end-to-end by the smoke test in
-- @ci.just@; this spec covers 'verdictCode' and 'verdictSummary'
-- against handcrafted outcome maps so the exit-code + summary-line
-- contracts are locked down without spinning up process-compose.
module CI.VerdictSpec (spec) where

import CI.CommitStatus (terminalToCommitStatus)
import CI.Gh (CommitStatus (Success))
import CI.ProcessCompose.Events (TerminalStatus)
import CI.Verdict (RecipeOutcome (..), terminalToOutcome, verdictCode, verdictSummary)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (ExitCode (..))
import Test.Hspec

-- The map literals below use bare string keys; @-XOverloadedStrings@
-- plus 'CI.Justfile.RecipeName's 'IsString' instance gives them the
-- right type. The verdict functions' signatures drive inference.

spec :: Spec
spec = do
  describe "verdictCode" $ do
    it "is ExitSuccess when every recipe succeeded" $
      verdictCode (Map.fromList [("a", Succeeded), ("b", Succeeded)])
        `shouldBe` ExitSuccess

    it "is ExitFailure 1 when any recipe Failed" $
      verdictCode (Map.fromList [("a", Succeeded), ("b", Failed)])
        `shouldBe` ExitFailure 1

    it "is ExitFailure 1 when any recipe was Skipped (dep failed)" $
      verdictCode (Map.fromList [("a", Succeeded), ("b", Skipped)])
        `shouldBe` ExitFailure 1

    it "is ExitFailure 1 when any recipe is still Unreported (never reached terminal state)" $
      verdictCode (Map.fromList [("a", Succeeded), ("b", Unreported)])
        `shouldBe` ExitFailure 1

    it "is ExitSuccess for the empty map" $
      verdictCode Map.empty `shouldBe` ExitSuccess

  describe "verdictSummary" $ do
    it "lists every recipe in the summary lines" $ do
      let recipes = [("alpha", Succeeded), ("beta", Failed), ("gamma", Skipped)]
          joined = T.unlines $ verdictSummary $ Map.fromList recipes
      for_ recipes $ \(n, _) ->
        (display n `T.isInfixOf` joined) `shouldBe` True

  -- Cross-module invariant: the two consumers of 'TerminalStatus'
  -- ('terminalToOutcome' in CI.Verdict, 'terminalToCommitStatus' in
  -- CI.CommitStatus) must agree on which terminal classification
  -- counts as "success". Without this, adding a new 'TerminalStatus'
  -- constructor and updating only one consumer would compile cleanly
  -- and produce a green GitHub check beside a red local exit (or
  -- vice versa). 'Bounded'/'Enum' on 'TerminalStatus' makes the
  -- enumeration future-proof against new constructors.
  describe "TerminalStatus" $
    it "terminalToOutcome and terminalToCommitStatus agree on the success case" $
      for_ [minBound .. maxBound :: TerminalStatus] $ \ts ->
        (terminalToOutcome ts == Succeeded)
          `shouldBe` (terminalToCommitStatus ts == Success)
