{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.Transport"'s runner-prefix selection. The shape of
the bundle+clone+run shell command is exercised end-to-end by the
@ci::run-check@ smoke test in @ci.just@; this spec locks down the
single classifier responsible for choosing between @ssh -T@ and
@pu connect@.
-}
module CI.TransportSpec (spec) where

import CI.Hosts (hostFromText)
import CI.Transport (remoteRunner)
import Test.Hspec

spec :: Spec
spec = describe "remoteRunner" $ do
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
