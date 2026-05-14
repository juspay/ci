{-# LANGUAGE OverloadedStrings #-}

-- | The per-run log path convention: @.ci\/\<short-sha\>\/\<recipe\>.log@.
-- One module owns the whole shape — directory name (SHA-keyed, 7-char
-- abbreviated for readability inside GitHub's 140-char commit-status
-- description) and per-recipe filename. A change to the convention
-- (longer SHA prefix to avoid collisions, a @\<system\>\/\<recipe\>@
-- shape per issue #14, a switch to S3 URLs) edits this file and
-- nothing else; the YAML emitter and the commit-status poster both
-- consume from here and stay byte-identical by construction.
module CI.LogPath (logDirFor, logPathFor) where

import CI.Git (Sha)
import qualified Data.Text as T
import Data.Text.Display (Display, display)
import System.FilePath ((</>))

-- | Compose the per-run log directory: @.ci\/\<short-sha\>\/@. Returns
-- a repo-relative path with a 7-char abbreviated SHA so the
-- @description@ field on a GitHub commit status stays readable inside
-- its 140-char budget (a 40-char hex blob blew most of it on one
-- field). Repo-relative so the path is portable across machines: a
-- contributor seeing the status can paste it straight into their own
-- checkout instead of looking at the runner's filesystem layout.
logDirFor :: Sha -> FilePath
logDirFor sha
  | T.length shaText < shortShaLen =
      error $
        "CI.LogPath.logDirFor: sha shorter than "
          <> show shortShaLen
          <> " chars: "
          <> T.unpack shaText
  | otherwise = ".ci" </> T.unpack (T.take shortShaLen shaText)
  where
    shaText = display sha
    shortShaLen = 7

-- | Compose @\<logDir\>\/\<recipe\>.log@. Lives alongside 'logDirFor'
-- so the full @.ci\/\<short-sha\>\/\<recipe\>.log@ convention is
-- owned by one module.
logPathFor :: Display a => FilePath -> a -> FilePath
logPathFor logDir recipe = logDir </> T.unpack (display recipe) <> ".log"
