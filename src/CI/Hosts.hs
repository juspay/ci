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

Two operations: 'loadHosts' (read the file, drop unknown keys),
and 'resolveHost' (look up a platform; prompt and persist on
miss). Prompting is only correct in interactive mode — strict
mode ('CI=true') must precede with a clean miss-test and die
before the run starts, because there's no TTY mid-run.
-}
module CI.Hosts (
    Host,
    hostFromText,
    Hosts,
    hostsPath,
    loadHosts,
    lookupHost,
    promptAndPersistHost,
)
where

import CI.Platform (Platform, parsePlatform)
import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stderr)

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
underlying map so 'resolveHost' / 'lookupHost' are the only access
points (no module-external pattern matching on the map shape).
-}
newtype Hosts = Hosts (Map Platform Host)
    deriving stock (Show)

{- | Absolute path to the hosts config: @\$HOME\/.config\/ci\/hosts.json@.
Computed once per process; the directory is auto-created on first
write via 'persistHost'.
-}
hostsPath :: IO FilePath
hostsPath = do
    home <- getHomeDirectory
    pure (home </> ".config" </> "ci" </> "hosts.json")

{- | Read the config file. Missing file → empty map (a fresh user has
no hosts yet, and that's not an error until something tries to
'resolveHost' a remote platform). Malformed JSON → 'fail'; we'd
rather refuse than silently drop the whole map.

Unknown platform keys are dropped silently — a future addition to
the 'Platform' enum shouldn't reject older configs, and an
already-deleted constructor in an older config shouldn't reject
newer binaries.
-}
loadHosts :: IO Hosts
loadHosts = do
    path <- hostsPath
    exists <- doesFileExist path
    if not exists
        then pure (Hosts Map.empty)
        else do
            bs <- BS.readFile path
            case eitherDecodeStrict @(Map Text Text) bs of
                Left err -> fail $ "ci: malformed " <> path <> ": " <> err
                Right raw ->
                    pure . Hosts . Map.fromList $
                        mapMaybe (\(k, v) -> (,Host v) <$> parsePlatform k) (Map.toList raw)

{- | Pure lookup; useful for offline checks (does this map have what
we need without prompting?).
-}
lookupHost :: Platform -> Hosts -> Maybe Host
lookupHost p (Hosts m) = Map.lookup p m

{- | Look up @p@; on miss, **prompt the user on @stderr@ for a host**,
read a line from @stdin@, persist the result back to the config
file, and return the new 'Host'. Reuses the prompt convention
kolu uses (one question per platform, persist on enter).
Name carries the interactive intent — every call site reads
@promptAndPersistHost@ in the chain, so non-interactive callers
(@--json@, MCP server, batch jobs) won't reach for it by accident.
Strict mode ('CI=true') has no TTY and stays on 'lookupHost' +
fail-fast at startup.
-}
promptAndPersistHost :: Platform -> Hosts -> IO (Host, Hosts)
promptAndPersistHost p hs@(Hosts m) = case Map.lookup p m of
    Just h -> pure (h, hs)
    Nothing -> do
        h <- promptHost p
        let m' = Map.insert p h m
        persistHosts (Hosts m')
        pure (h, Hosts m')

{- | Single-line prompt on @stderr@ (stdout stays clean for pipeable
output), read one line from stdin, strip whitespace, refuse the
empty answer with a re-prompt loop.
-}
promptHost :: Platform -> IO Host
promptHost p = loop
  where
    loop = do
        TIO.hPutStr stderr $ "ci: SSH host for " <> display p <> "? "
        hFlush stderr
        line <- T.strip <$> TIO.getLine
        if T.null line
            then do
                TIO.hPutStrLn stderr "  (empty input — please enter a hostname)"
                loop
            else pure (Host line)

{- | Write the full 'Hosts' map back to disk. Creates the parent
directory on first write. Atomic via @writeFile@'s
truncate-and-write semantics — we don't bother with a tmpfile
rename because two concurrent @ci@ invocations are already
blocked by other means (see #10), so the only writer is this
process.
-}
persistHosts :: Hosts -> IO ()
persistHosts (Hosts m) = do
    path <- hostsPath
    createDirectoryIfMissing True (takeDirectory path)
    let raw :: Map Text Text
        raw = Map.fromList [(display p, display h) | (p, h) <- Map.toList m]
    result :: Either IOException () <- try $ BSL.writeFile path (encode raw)
    case result of
        Right () -> pure ()
        Left e ->
            TIO.hPutStrLn stderr $
                "ci: warning: failed to persist " <> T.pack path <> ": " <> T.pack (show e)
