{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Node"'s 'Display' / 'parseNodeId' round-trip.
-- This is the single wire-to-domain seam for process-compose's
-- process-name strings, so the contract is locked here: every
-- displayable 'NodeId' round-trips, and unparseable input drops
-- silently (a @Nothing@) rather than crashing the consumer.
module CI.NodeSpec (spec) where

import CI.Node (NodeId (..), parseNodeId)
import CI.Platform (Platform (..))
import Data.Text.Display (display)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNodeId" $ do
    it "parses <recipe>@x86_64-linux into NodeId" $
      parseNodeId "build@x86_64-linux" `shouldBe` Just (NodeId "build" X86_64Linux)

    it "parses <recipe>@aarch64-darwin into NodeId" $
      parseNodeId "build@aarch64-darwin" `shouldBe` Just (NodeId "build" Aarch64Darwin)

    it "preserves :: in recipe FQNs" $
      parseNodeId "sub::build@x86_64-linux" `shouldBe` Just (NodeId "sub::build" X86_64Linux)

    it "splits on the last @ (recipes never contain @, platforms never contain ::)" $
      parseNodeId "a::b::c@aarch64-darwin" `shouldBe` Just (NodeId "a::b::c" Aarch64Darwin)

    it "returns Nothing on missing platform suffix" $
      parseNodeId "build" `shouldBe` Nothing

    it "returns Nothing on unknown platform" $
      parseNodeId "build@windows" `shouldBe` Nothing

    it "returns Nothing on empty recipe name" $
      parseNodeId "@x86_64-linux" `shouldBe` Nothing

  describe "Display NodeId" $ do
    it "emits <recipe>@<platform>" $
      display (NodeId "build" X86_64Linux) `shouldBe` "build@x86_64-linux"

    it "preserves :: in recipe FQNs" $
      display (NodeId "sub::build" Aarch64Darwin) `shouldBe` "sub::build@aarch64-darwin"

  describe "round-trip" $ do
    let cases =
          [ NodeId "build" X86_64Linux,
            NodeId "build" Aarch64Darwin,
            NodeId "sub::nested::recipe" X86_64Linux,
            NodeId "a-recipe-with-dashes" Aarch64Darwin
          ]
    it "parseNodeId . display is Just for every NodeId" $
      mapM_
        (\n -> parseNodeId (display n) `shouldBe` Just n)
        cases
