{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.Transport"'s runner-prefix selection and the
arch-aware switch between same-arch (closure-copy + absolute @just@
path) and foreign-arch (bare @just@ on remote PATH) command shapes.
The bundle+clone+run shell command is exercised end-to-end by the
@ci::run-check@ smoke test in @ci.just@; this spec locks down the
classifier choices in isolation.
-}
module CI.TransportSpec (spec) where

import CI.Git (shaPlaceholder)
import CI.Hosts (hostFromText)
import CI.Platform (Platform (..))
import CI.Transport (commandFor, remoteRunner, sshTransport)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "remoteRunner" $ do
        it "wraps a plain hostname in `ssh -T`" $
            remoteRunner (hostFromText "sincereintent") `shouldBe` "ssh -T sincereintent"

        it "wraps a user@host form in `ssh -T`" $
            remoteRunner (hostFromText "srid@builder.example.com") `shouldBe` "ssh -T srid@builder.example.com"

        it "uses a `pu connect <name>` host verbatim, no ssh prefix" $
            remoteRunner (hostFromText "pu connect builder-mac") `shouldBe` "pu connect builder-mac"

        it "preserves trailing args inside the pu prefix" $
            -- A user might write `pu connect mybox --instance foo` if pu accepts
            -- extra flags; treat the whole string as the runner prefix.
            remoteRunner (hostFromText "pu connect mybox --some-flag")
                `shouldBe` "pu connect mybox --some-flag"

        it "does not treat a similar-looking prefix as pu (must be the exact `pu connect ` token)" $
            -- `pulse-host` starts with `pu` but is not the incus prefix; falls
            -- through to ssh.
            remoteRunner (hostFromText "pulse-host") `shouldBe` "ssh -T pulse-host"

    describe "commandFor (sshTransport ... localPlat targetPlat)" $ do
        let host = hostFromText "remote.example.com"
            sha = shaPlaceholder
            recipe = "ci::build"
            -- Same-platform pair: sshTransport classifies as NativeArch
            -- at construction; commandFor emits the closure-copy +
            -- absolute-path shape.
            native = commandFor (sshTransport host sha Linux Linux) recipe
            -- Cross-platform pair: sshTransport classifies as ForeignArch
            -- at construction; commandFor skips the closure-copy step.
            foreign_ = commandFor (sshTransport host sha Linux Macos) recipe

        it "same-platform target prepends a `nix-store --export | runner nix-store --import` step" $
            ("nix-store --export" `T.isInfixOf` native) `shouldBe` True

        it "same-platform target invokes just by absolute /nix/store path" $
            ("/nix/store/" `T.isInfixOf` native) `shouldBe` True

        it "cross-platform target omits the nix-store --export step" $
            ("nix-store --export" `T.isInfixOf` foreign_) `shouldBe` False

        it "cross-platform target invokes bare `just --no-deps`" $
            ("just --no-deps ci::build" `T.isInfixOf` foreign_) `shouldBe` True
