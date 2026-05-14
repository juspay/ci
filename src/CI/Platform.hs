{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The CI fanout platform vocabulary. A 'Platform' identifies one lane
the pipeline can be expanded across — a host OS the pipeline is willing
to send recipes to (locally or via SSH). Distinct from 'CI.Justfile.Os'
(which is just's full host-OS-gate vocabulary, including Unix, BSDs,
Windows): 'Platform' is the strictly smaller set we route to.

The renderings here ('display', 'parsePlatform') are the *single*
source of truth — they're consumed by every downstream string-shaped
caller (the YAML process-map key via 'CI.Node.NodeId', the
@\<platform\>@ segment of log paths, the host-config JSON keys).
-}
module CI.Platform (
    Platform (..),
    allPlatforms,
    parsePlatform,
    osToPlatform,
    LocalPlatformError,
    localPlatform,
)
where

import qualified CI.Justfile as J
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import qualified System.Info

{- | The fanout platform set. Closed sum on purpose: every consumer that
pattern-matches on 'Platform' becomes a pattern-match exhaustiveness
warning when a new constructor lands, instead of a silent omission.
-}
data Platform = Linux | Macos
    deriving stock (Show, Eq, Ord, Bounded, Enum)

{- | Lowercase, no qualifier. Matches @CI_SYSTEM@-style conventions and
the JSON keys in @~\/.config\/ci\/hosts.json@.
-}
instance Display Platform where
    displayBuilder Linux = "linux"
    displayBuilder Macos = "macos"

{- | Every 'Platform'. Used by the hosts-config loader to enumerate
valid keys without listing constructors by hand.
-}
allPlatforms :: [Platform]
allPlatforms = [minBound .. maxBound]

{- | Inverse of 'display' on the closed set. 'Nothing' on anything
else — callers (host-config loader, 'CI.Node.parseNodeId') tolerate
the failure rather than dying, so an unknown key in hosts.json or
a future process-compose event with a different platform tag drops
on the floor instead of tearing the run down.
-}
parsePlatform :: Text -> Maybe Platform
parsePlatform t = case T.toLower t of
    "linux" -> Just Linux
    "macos" -> Just Macos
    _ -> Nothing

{- | Bridge from just's full host-OS-gate vocabulary
('CI.Justfile.Os') to the strictly smaller fanout vocabulary
here. 'Nothing' for gates that don't identify a CI lane target
(Unix, Windows, the BSDs) — those stay host-OS gates only, never
fanout targets. This is the *only* coupling between the two
vocabularies; downstream consumers route through 'Platform'.
-}
osToPlatform :: J.Os -> Maybe Platform
osToPlatform J.Linux = Just Linux
osToPlatform J.Macos = Just Macos
osToPlatform _ = Nothing

{- | The host wasn't a 'Platform' we know how to route to. Today only
@linux@ and @darwin@ (per 'System.Info.os') resolve; the orchestrator
refuses to guess.
-}
newtype LocalPlatformError = LocalPlatformError {hostOs :: String}
    deriving stock (Show)

instance Display LocalPlatformError where
    displayBuilder e =
        "unsupported host OS for CI: "
            <> displayBuilder (T.pack e.hostOs)
            <> " (only linux and macos are supported)"

{- | Classify the running host into a 'Platform'. Reads
'System.Info.os' (compiled into the binary by GHC, no shell-out);
maps @"linux"@ to 'Linux' and @"darwin"@ to 'Macos'.

Pure: 'System.Info.os' is a compile-time constant baked in by GHC,
so this is a 'Either', not an 'IO Either'. The earlier 'IO' wrapper
was cosmetic ("symmetry with other resolvers") and forced every
caller into '<<=' threading for no runtime gain; readers of the
type now see the truth (no effects, two invocations always agree).
-}
localPlatform :: Either LocalPlatformError Platform
localPlatform = case System.Info.os of
    "linux" -> Right Linux
    "darwin" -> Right Macos
    other -> Left (LocalPlatformError other)
