{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.Node"'s 'Display' / 'parseNodeId' round-trip.
This is the single wire-to-domain seam for process-compose's
process-name strings, so the contract is locked here: every
displayable 'NodeId' round-trips, and unparseable input drops
silently (a @Nothing@) rather than crashing the consumer.
-}
module CI.NodeSpec (spec) where

import CI.Node (NodeId (..), parseNodeId)
import CI.Platform (Platform (..))
import Data.Text.Display (display)
import Test.Hspec

spec :: Spec
spec = do
    describe "parseNodeId" $ do
        it "parses <recipe>@linux into NodeId" $
            parseNodeId "build@linux" `shouldBe` Just (NodeId "build" Linux)

        it "parses <recipe>@macos into NodeId" $
            parseNodeId "build@macos" `shouldBe` Just (NodeId "build" Macos)

        it "preserves :: in recipe FQNs" $
            parseNodeId "sub::build@linux" `shouldBe` Just (NodeId "sub::build" Linux)

        it "splits on the last @ (recipes never contain @, platforms never contain ::)" $
            parseNodeId "a::b::c@macos" `shouldBe` Just (NodeId "a::b::c" Macos)

        it "returns Nothing on missing platform suffix" $
            parseNodeId "build" `shouldBe` Nothing

        it "returns Nothing on unknown platform" $
            parseNodeId "build@windows" `shouldBe` Nothing

        it "returns Nothing on empty recipe name" $
            parseNodeId "@linux" `shouldBe` Nothing

    describe "Display NodeId" $ do
        it "emits <recipe>@<platform>" $
            display (NodeId "build" Linux) `shouldBe` "build@linux"

        it "preserves :: in recipe FQNs" $
            display (NodeId "sub::build" Macos) `shouldBe` "sub::build@macos"

    describe "round-trip" $ do
        let cases =
                [ NodeId "build" Linux
                , NodeId "build" Macos
                , NodeId "sub::nested::recipe" Linux
                , NodeId "a-recipe-with-dashes" Macos
                ]
        it "parseNodeId . display is Just for every NodeId" $
            mapM_
                (\n -> parseNodeId (display n) `shouldBe` Just n)
                cases
