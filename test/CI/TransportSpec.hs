{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Transport"'s three command builders and runner
-- prefix. The end-to-end bundle+clone+run path is exercised by the
-- @ci::run-check@ smoke test in @ci.just@; this spec locks down the
-- structural choices in isolation.
module CI.TransportSpec (spec) where

import CI.Git (shaPlaceholder)
import CI.Hosts (hostFromText)
import CI.Platform (Platform (..))
import CI.Transport (localRecipeCommand, remoteRunner, sshRecipeCommand, sshSetupCommand)
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

  describe "localRecipeCommand" $ do
    it "emits a bare just --no-deps invocation (pc working_dir handles the cwd)" $
      ("--no-deps ci::build" `T.isInfixOf` localRecipeCommand "ci::build") `shouldBe` True

  describe "sshSetupCommand" $ do
    let host = hostFromText "remote.example.com"
        sha = shaPlaceholder
        cmd = sshSetupCommand host sha Aarch64Darwin

    it "ships the just derivation first" $
      ("nix-store --export" `T.isInfixOf` cmd) `shouldBe` True

    it "bundles HEAD into the remote cache dir" $
      ("git bundle create" `T.isInfixOf` cmd) `shouldBe` True

    it "clones into the per-(sha,platform) cached run dir on the remote" $
      ("$HOME/.cache/ci/0000000/aarch64-darwin" `T.isInfixOf` cmd) `shouldBe` True

    it "skips bundle+clone on cache hit" $
      ("cat > /dev/null; exit 0" `T.isInfixOf` cmd) `shouldBe` True

  describe "sshRecipeCommand" $ do
    let host = hostFromText "remote.example.com"
        sha = shaPlaceholder
        cmd = sshRecipeCommand host sha Aarch64Darwin "ci::build"

    it "cd's into the per-(sha,platform) cached run dir set up by the setup node" $
      ("cd $HOME/.cache/ci/0000000/aarch64-darwin/src" `T.isInfixOf` cmd) `shouldBe` True

    it "realises the drv on the remote and invokes /bin/just" $
      ("$(nix-store --realise" `T.isInfixOf` cmd) `shouldBe` True

    it "ends with --no-deps + the recipe" $
      ("/bin/just --no-deps ci::build" `T.isInfixOf` cmd) `shouldBe` True

    it "does not re-bundle (setup did that)" $
      ("git bundle" `T.isInfixOf` cmd) `shouldBe` False
