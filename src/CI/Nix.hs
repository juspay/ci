{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- The helper used inside the TH splices below ('staticDrvPath') lives
-- in @CI.Nix.Splice@ because TH's stage restriction forbids a splice
-- from calling a function defined in the same module.

{- | Compile-time Nix-derivation references and the shell snippets
that ship them across.

'justDrvFor' is the analogue of 'System.Which.staticWhich' for a
@.drv@ path (rather than an output path) — one per supported
'CI.Platform.Platform', baked in at TH-splice time from the
@CI_JUST_DRV_\<system\>@ env vars the flake injects. The drv
files themselves materialise as a side effect of flake
evaluation, so the local Nix store has them ready for @nix-store
--export@ at runtime.

Two consumers, both in 'CI.Transport':

  * 'shipJustDrv' — the @nix-store --export ... | <runner>
    nix-store --import@ step that pushes the closure of the just
    derivation for a target platform to a remote.

  * 'realisedJust' — the @\\$(nix-store --realise <drv>!out)@
    expansion that, on the remote, fetches/builds the native
    binary for that platform and yields the @bin/just@ prefix.

Keeping both shell snippets here means every concrete Nix-CLI
invocation (export, import, realise, output-selector syntax) is
in one module instead of woven into the bundle+ssh choreography
of 'CI.Transport'.

If the env var is unset at compile time (e.g. plain @cabal build@
outside the flake), the splice emits a clearly-broken placeholder
so the build still succeeds and the failure mode at runtime is a
recognisable "invalid store path" error.
-}
module CI.Nix (justDrvFor, shipJustDrv, realisedJust) where

import CI.Justfile (RecipeName)
import CI.Nix.Splice (staticDrvPath)
import CI.Platform (Platform (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)

-- | @/nix/store/...drv@ for @just@ on @x86_64-linux@, baked in.
justDrvX86_64Linux :: FilePath
justDrvX86_64Linux = $(staticDrvPath "CI_JUST_DRV_X86_64_LINUX")

-- | @/nix/store/...drv@ for @just@ on @aarch64-linux@, baked in.
justDrvAarch64Linux :: FilePath
justDrvAarch64Linux = $(staticDrvPath "CI_JUST_DRV_AARCH64_LINUX")

-- | @/nix/store/...drv@ for @just@ on @x86_64-darwin@, baked in.
justDrvX86_64Darwin :: FilePath
justDrvX86_64Darwin = $(staticDrvPath "CI_JUST_DRV_X86_64_DARWIN")

-- | @/nix/store/...drv@ for @just@ on @aarch64-darwin@, baked in.
justDrvAarch64Darwin :: FilePath
justDrvAarch64Darwin = $(staticDrvPath "CI_JUST_DRV_AARCH64_DARWIN")

{- | Drv path for @just@ on the given target platform — the recipe
shipped to that remote, realised on-site to produce a natively
executable binary regardless of the runner's own arch.
-}
justDrvFor :: Platform -> FilePath
justDrvFor X86_64Linux = justDrvX86_64Linux
justDrvFor Aarch64Linux = justDrvAarch64Linux
justDrvFor X86_64Darwin = justDrvX86_64Darwin
justDrvFor Aarch64Darwin = justDrvAarch64Darwin

{- | Shell snippet that copies the @just@ derivation for the given
target platform to a remote, via the runner-command prefix (e.g.
@ssh -T hostname@).

@nix-store --export $(...closure...) | <runner> nix-store --import@
ships the drv file plus its closure; the remote can then
@--realise@ it. Output is redirected to @/dev/null@ since
@nix-store --import@ prints every imported path on its own line
and we don't need that noise in the per-node log.
-}
shipJustDrv :: Text -> Platform -> Text
shipJustDrv runner targetPlat =
    "nix-store --export $(nix-store --query --requisites "
        <> drv
        <> ") | "
        <> runner
        <> " nix-store --import > /dev/null"
  where
    drv = T.pack (justDrvFor targetPlat)

{- | The bash sub-expression that yields the @just --no-deps
\<recipe\>@ invocation on the remote, with @just@ provided by
realising the platform-specific drv.

@nix-store --realise <drv>!out@ selects the @out@ output of a
multi-output derivation (just has @out@, @man@, @doc@) so the
expansion is exactly one store path, regardless of how many
outputs the derivation declares.

The leading @\\$@ is escaped so the @$()@ subshell is evaluated
by the *remote* shell (after ssh argv substitution) rather than
the local one.
-}
realisedJust :: Platform -> RecipeName -> Text
realisedJust targetPlat recipe =
    "\\$(nix-store --realise "
        <> drv
        <> "!out)/bin/just --no-deps "
        <> display recipe
  where
    drv = T.pack (justDrvFor targetPlat)
