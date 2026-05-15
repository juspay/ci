{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.Transport"'s runner-prefix selection and the
drv-copy + realise remote command shape. The end-to-end
bundle+clone+run path is exercised by the @ci::run-check@ smoke
test in @ci.just@; this spec locks down the structural choices in
isolation.
-}
module CI.TransportSpec (spec) where

import CI.Git (shaPlaceholder)
import CI.Hosts (hostFromText)
import CI.Platform (Platform (..))
import CI.Transport (Transport (..), commandFor, remoteRunner)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "remoteRunner" $ do
        it "wraps a plain hostname in `ssh -T`" $
            remoteRunner (hostFromText "sincereintent") `shouldBe` "ssh -T sincereintent"

        it "wraps a user@host form in `ssh -T`" $
            remoteRunner (hostFromText "srid@builder.example.com") `shouldBe` "ssh -T srid@builder.example.com"

        it "treats an ssh-config alias the same — anything ssh dials works" $
            remoteRunner (hostFromText "srid1") `shouldBe` "ssh -T srid1"

    describe "commandFor (Ssh ... targetPlat)" $ do
        let host = hostFromText "remote.example.com"
            sha = shaPlaceholder
            recipe = "ci::build"
            cmd = commandFor (Ssh host sha Aarch64Darwin) recipe

        it "ships the just derivation via nix-store --export | nix-store --import" $
            ("nix-store --export" `T.isInfixOf` cmd) `shouldBe` True

        it "realises the drv on the remote (escaped $ so the subshell runs there, not locally)" $
            ("\\$(nix-store --realise" `T.isInfixOf` cmd) `shouldBe` True

        it "invokes just by the realised /bin/just path with --no-deps + the recipe" $
            ("/bin/just --no-deps ci::build" `T.isInfixOf` cmd) `shouldBe` True

        it "uses the runner prefix for every remote call" $
            ("ssh -T remote.example.com" `T.isInfixOf` cmd) `shouldBe` True
