{-# LANGUAGE TemplateHaskell #-}

-- The helper used inside the TH splices below ('staticDrvPath') lives
-- in @CI.Nix.Splice@ because TH's stage restriction forbids a splice
-- from calling a function defined in the same module.

{- | Compile-time Nix-derivation references. Analogous to
'System.Which.staticWhich' (which bakes an absolute @/nix/store@
*output* path into the binary at TH-splice time), 'staticDrvPath'
bakes an absolute @/nix/store@ *derivation* path read from a
build-environment env var.

The flake declares one @CI_JUST_DRV_\<system\>@ env var per
'CI.Platform.Platform' constructor; this module turns each into a
type-safe lookup. The runner ships these drv files (not the
outputs) to remotes via @nix-store --export | nix-store --import@;
the remote @nix-store --realise@s on-site, hitting its own
substituter chain (typically @cache.nixos.org@) for the native
binary. That lets a linux runner provide a working @just@ on an
aarch64-darwin remote without any pre-installed PATH dependency
beyond Nix itself.

If the env var is unset at compile time (e.g. plain @cabal build@
outside the flake), the splice emits a clearly-broken placeholder
so the build still succeeds and the failure mode at runtime is a
recognisable "invalid store path" error.
-}
module CI.Nix (justDrvFor) where

import CI.Nix.Splice (staticDrvPath)
import CI.Platform (Platform (..))

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
