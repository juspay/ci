{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Verdict"'s pure verdict logic. The 'recordOutcome'
-- side of the module is exercised end-to-end by the smoke test in
-- @ci.just@; this spec covers 'verdictCode' and 'verdictSummary'
-- against handcrafted outcome maps so the exit-code + summary-line
-- contracts are locked down without spinning up process-compose.
module CI.VerdictSpec (spec) where

import CI.CommitStatus (terminalToCommitStatus)
import CI.Gh (CommitStatus (Success))
import CI.Justfile (RecipeName)
import CI.Node (NodeId (..))
import CI.Platform (Platform (..))
import CI.ProcessCompose.Events (TerminalStatus)
import CI.Verdict (RecipeOutcome (..), terminalToOutcome, verdictCode, verdictSummary)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (display)
import System.Exit (ExitCode (..))
import Test.Hspec

-- | Convenience: build a X86_64Linux-lane 'RecipeNode' from a bare
-- recipe-name string literal. 'CI.Justfile.RecipeName' has
-- 'IsString', so the argument under @-XOverloadedStrings@
-- disambiguates correctly.
nodeLinux :: RecipeName -> NodeId
nodeLinux r = RecipeNode r X86_64Linux

spec :: Spec
spec = do
  describe "verdictCode" $ do
    it "is ExitSuccess when every node succeeded" $
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Just Succeeded)])
        `shouldBe` ExitSuccess

    it "is ExitFailure 1 when any node failed" $
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Just Failed)])
        `shouldBe` ExitFailure 1

    it "is ExitFailure 1 when any node never reached terminal (Nothing)" $
      verdictCode (Map.fromList [(nodeLinux "a", Just Succeeded), (nodeLinux "b", Nothing)])
        `shouldBe` ExitFailure 1

    it "is ExitSuccess for the empty map" $
      verdictCode Map.empty `shouldBe` ExitSuccess

  describe "verdictSummary" $ do
    it "lists every node in the summary lines" $ do
      let nodes = [(nodeLinux "alpha", Just Succeeded), (nodeLinux "beta", Just Failed), (nodeLinux "gamma", Nothing)]
          joined = T.unlines $ verdictSummary (const "local") $ Map.fromList nodes
      for_ nodes $ \(n, _) ->
        (display n `T.isInfixOf` joined) `shouldBe` True

    it "renders Nothing as 'did not run'" $ do
      let nodes = [(nodeLinux "alpha", Nothing)]
          joined = T.unlines $ verdictSummary (const "local") $ Map.fromList nodes
      ("did not run" `T.isInfixOf` joined) `shouldBe` True

    it "shows the platform suffix in each summary line" $ do
      let nodes = [(RecipeNode "alpha" X86_64Linux, Just Succeeded), (RecipeNode "alpha" Aarch64Darwin, Just Failed)]
          joined = T.unlines $ verdictSummary (const "local") $ Map.fromList nodes
      ("alpha@x86_64-linux" `T.isInfixOf` joined) `shouldBe` True
      ("alpha@aarch64-darwin" `T.isInfixOf` joined) `shouldBe` True

    -- Synthetic setup nodes (per-platform bundle ship / drv copy) are
    -- internal plumbing and must not appear in the user-facing
    -- summary — matching 'CI.CommitStatus.seedPending' /
    -- 'postStatusFor', which already skip them from GH posts.
    it "omits setup nodes from the per-node lines" $ do
      let nodes =
            [ (SetupNode X86_64Linux, Just Succeeded),
              (RecipeNode "build" X86_64Linux, Just Succeeded)
            ]
          joined = T.unlines $ verdictSummary (const "local") $ Map.fromList nodes
      ("_ci-setup" `T.isInfixOf` joined) `shouldBe` False
      ("build@x86_64-linux" `T.isInfixOf` joined) `shouldBe` True

    it "omits setup nodes from the n-of-m count" $ do
      let nodes =
            [ (SetupNode X86_64Linux, Just Succeeded),
              (RecipeNode "build" X86_64Linux, Just Succeeded),
              (RecipeNode "test" X86_64Linux, Just Succeeded)
            ]
          joined = T.unlines $ verdictSummary (const "local") $ Map.fromList nodes
      -- Two user recipes, not three (setup omitted).
      ("all 2 nodes succeeded" `T.isInfixOf` joined) `shouldBe` True

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
