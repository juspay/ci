{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.Platform"'s 'Display' / 'parsePlatform' surface
and the just-OS → 'Platform' bridge. The 'localPlatform' resolver
isn't exercised here (its 'System.Info.os' input is compiled-in,
not parameterizable) — the run-check smoke test in @ci.just@
covers it end-to-end.
-}
module CI.PlatformSpec (spec) where

import qualified CI.Justfile as J
import CI.Platform (Platform (..), allPlatforms, osToPlatform, parsePlatform)
import Data.Text.Display (display)
import Test.Hspec

spec :: Spec
spec = do
    describe "parsePlatform" $ do
        it "parses linux" $ parsePlatform "linux" `shouldBe` Just Linux
        it "parses macos" $ parsePlatform "macos" `shouldBe` Just Macos
        it "is case-insensitive" $ parsePlatform "MACOS" `shouldBe` Just Macos
        it "rejects windows" $ parsePlatform "windows" `shouldBe` Nothing
        it "rejects empty" $ parsePlatform "" `shouldBe` Nothing

    describe "Display Platform" $ do
        it "renders linux" $ display Linux `shouldBe` "linux"
        it "renders macos" $ display Macos `shouldBe` "macos"

    describe "allPlatforms" $
        it "enumerates every constructor" $
            allPlatforms `shouldBe` [Linux, Macos]

    describe "osToPlatform" $ do
        it "bridges just's Linux Os to Linux" $
            osToPlatform J.Linux `shouldBe` Just Linux
        it "bridges just's Macos Os to Macos" $
            osToPlatform J.Macos `shouldBe` Just Macos
        it "rejects Unix gates (not a fanout target)" $
            osToPlatform J.Unix `shouldBe` Nothing
        it "rejects Windows (not a fanout target)" $
            osToPlatform J.Windows `shouldBe` Nothing
        it "rejects BSDs (not fanout targets)" $ do
            osToPlatform J.Freebsd `shouldBe` Nothing
            osToPlatform J.Openbsd `shouldBe` Nothing
            osToPlatform J.Netbsd `shouldBe` Nothing
            osToPlatform J.Dragonfly `shouldBe` Nothing
