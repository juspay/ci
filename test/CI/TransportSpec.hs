{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.Transport"'s runner-prefix selection and the
two remote-command shapes (setup + per-recipe). The end-to-end
bundle+clone+run path is exercised by the @ci::run-check@ smoke
test in @ci.just@; this spec locks down the structural choices
in isolation.
-}
module CI.TransportSpec (spec) where

import CI.Git (shaPlaceholder)
import CI.Hosts (hostFromText)
import CI.Platform (Platform (..))
import CI.Transport (CommandShape (..), Transport (..), cachedRunDir, commandFor, remoteRunner)
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

    describe "cachedRunDir" $
        it "uses ~/.cache/ci/<short-sha>/<platform> on the remote" $
            cachedRunDir shaPlaceholder Aarch64Darwin `shouldBe` "$HOME/.cache/ci/0000000/aarch64-darwin"

    describe "commandFor Ssh + SetupCommand" $ do
        let host = hostFromText "remote.example.com"
            sha = shaPlaceholder
            -- Setup-vs-recipe is discriminated by 'CommandShape',
            -- chosen by the caller in 'CI.Pipeline'; Transport
            -- doesn't read node identity to re-derive the choice.
            cmd = commandFor (Ssh host sha Aarch64Darwin) SetupCommand

        it "ships the just derivation first" $
            ("nix-store --export" `T.isInfixOf` cmd) `shouldBe` True

        it "bundles HEAD into the remote cache dir" $
            ("git bundle create" `T.isInfixOf` cmd) `shouldBe` True

        it "clones into the cached run dir on the remote" $
            ("$HOME/.cache/ci/" `T.isInfixOf` cmd) `shouldBe` True

        it "skips bundle+clone on cache hit" $
            ("cat > /dev/null; exit 0" `T.isInfixOf` cmd) `shouldBe` True

    describe "commandFor Ssh + RecipeCommand" $ do
        let host = hostFromText "remote.example.com"
            sha = shaPlaceholder
            cmd = commandFor (Ssh host sha Aarch64Darwin) (RecipeCommand "ci::build")

        it "cd's into the cached run dir set up by the setup node" $
            ("cd $HOME/.cache/ci/" `T.isInfixOf` cmd) `shouldBe` True

        it "realises the drv on the remote and invokes /bin/just" $
            ("$(nix-store --realise" `T.isInfixOf` cmd) `shouldBe` True

        it "ends with --no-deps + the recipe" $
            ("/bin/just --no-deps ci::build" `T.isInfixOf` cmd) `shouldBe` True

        it "does not re-bundle (setup did that)" $
            ("git bundle" `T.isInfixOf` cmd) `shouldBe` False
