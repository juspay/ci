{-# LANGUAGE TemplateHaskell #-}

{- | Template Haskell helpers for "CI.Nix". Split into its own module
because TH's stage restriction forbids a splice from referring to
a helper defined in the same module — a build-time function used
inside a splice must live in a dependency, not a peer.
-}
module CI.Nix.Splice (staticDrvPath) where

import Language.Haskell.TH (Exp, Q, runIO, stringE)
import System.Environment (lookupEnv)

{- | Splice in a @/nix/store/...drv@ path baked at compile time
from the given env var. The env vars are set by flake.nix via
haskell-flake's build-time settings, one per supported Nix system.

@runIO (lookupEnv ...)@ runs at TH-splice time. Missing or empty →
clearly-broken placeholder (avoids forcing every dev @cabal build@
to go through flake.nix); runtime use of the placeholder fails
fast with a recognisable error from @nix-store@.
-}
staticDrvPath :: String -> Q Exp
staticDrvPath envVar = do
    result <- runIO (lookupEnv envVar)
    case result of
        Just p | not (null p) -> stringE p
        _ -> stringE $ "/nix/store/__" <> envVar <> "_NOT_SET__-just.drv"
