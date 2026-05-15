{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{- | Persistent map from 'CI.Platform.Platform' to the SSH 'Host' the
runner should use for that lane. Lives at
@~\/.config\/ci\/hosts.json@ — one global file per user, shared
across every repo on this machine. Same convention kolu uses.

Read-only from the runner's perspective: the user edits the JSON
file by hand. 'loadHosts' reads the file (dropping unknown keys),
'lookupHost' / 'hostsPlatforms' query the result. Missing entries
are not an error — 'CI.Pipeline.pipelinePlatformsFor' silently
excludes platforms with no entry from the fanout, so the user
opts in to a remote lane by adding its hosts.json key.
-}
module CI.Hosts (
    Host,
    hostFromText,
    Hosts,
    hostsPath,
    loadHosts,
    lookupHost,
    hostsPlatforms,
)
where

import CI.Platform (Platform, parsePlatform)
import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Display (Display)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

{- | An SSH destination — anything @ssh@ accepts as @[user@]host[:port]@.
Opaque; minted only by 'hostFromText' or via JSON decode in 'loadHosts'.
-}
newtype Host = Host Text
    deriving stock (Show, Eq, Ord)
    deriving newtype (Display)

{- | Smart constructor — named so every minting site is searchable. No
validation today; the @ssh@ subprocess is the source of truth on
whether the string is a valid destination.
-}
hostFromText :: Text -> Host
hostFromText = Host

{- | A loaded view of @~\/.config\/ci\/hosts.json@. Newtype around the
underlying map so 'lookupHost' and 'hostsPlatforms' are the only
access points (no module-external pattern matching on the map shape).
-}
newtype Hosts = Hosts (Map Platform Host)
    deriving stock (Show)

{- | Absolute path to the hosts config: @\$HOME\/.config\/ci\/hosts.json@.
Computed once per process. The runner only reads the file; the
user creates it.
-}
hostsPath :: IO FilePath
hostsPath = do
    home <- getHomeDirectory
    pure (home </> ".config" </> "ci" </> "hosts.json")

{- | Read the config file. Missing file → empty map (a fresh user has
no hosts yet, and that's not an error — 'pipelinePlatformsFor'
filters non-local platforms without entries out of the fanout).
Malformed JSON → 'fail'; we'd rather refuse than silently drop
the whole map.

Unknown platform keys are dropped silently — a future addition to
the 'Platform' enum shouldn't reject older configs, and an
already-deleted constructor in an older config shouldn't reject
newer binaries.
-}
loadHosts :: IO Hosts
loadHosts = do
    path <- hostsPath
    result <- try @IOException $ BS.readFile path
    case result of
        Left e | isDoesNotExistError e -> pure (Hosts Map.empty)
        Left e -> fail $ "ci: cannot read " <> path <> ": " <> show e
        Right bs ->
            case eitherDecodeStrict @(Map Text Text) bs of
                Left err -> fail $ "ci: malformed " <> path <> ": " <> err
                Right raw ->
                    pure . Hosts . Map.fromList $
                        mapMaybe (\(k, v) -> (,Host v) <$> parsePlatform k) (Map.toList raw)

-- | Pure lookup.
lookupHost :: Platform -> Hosts -> Maybe Host
lookupHost p (Hosts m) = Map.lookup p m

{- | Every 'Platform' with a configured host entry. The pipeline
fanout in 'CI.Pipeline' intersects this set with the root recipe's
declared OS families to decide which Nix systems to target — so a
platform without a hosts.json entry doesn't appear in the fanout
at all (no prompt-on-miss, no fail-fast: the user explicitly opts
in by writing the file).
-}
hostsPlatforms :: Hosts -> [Platform]
hostsPlatforms (Hosts m) = Map.keys m
