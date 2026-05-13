{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | GitHub CLI (@gh@) operations. Today: discover @\<owner\>/\<repo\>@ from
-- the current checkout. The @gh api@ POST that posts the commit status
-- itself lives in "CI.CommitStatus"; both modules share 'ghBin' from here.
module CI.Gh
  ( -- * Binary
    ghBin,

    -- * Values
    RepoCoords (..),

    -- * Operations
    GhError (..),
    resolveRepoCoords,
  )
where

import CI.Subprocess (SubprocessError, runSubprocess)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Which (staticWhich)

-- | Absolute path to the @gh@ binary, baked in at compile time via Nix.
ghBin :: FilePath
ghBin = $(staticWhich "gh")

-- | The two halves of a GitHub repository identifier, as in
-- @\<owner\>/\<repo\>@. Resolved once from @gh repo view@ and threaded into
-- 'CI.CommitStatus.postStatus' alongside the 'CI.Git.Sha'.
data RepoCoords = RepoCoords {owner :: Text, repo :: Text}
  deriving stock (Show, Eq)

-- | Failures from the gh operations in this module.
data GhError
  = GhSubprocess SubprocessError
  | UnexpectedNameWithOwner String
  deriving stock (Show)

instance Display GhError where
  displayBuilder (GhSubprocess e) = displayBuilder e
  displayBuilder (UnexpectedNameWithOwner out) =
    "unexpected nameWithOwner from gh: " <> displayBuilder (T.pack out)

-- | Resolve the @\<owner\>/\<repo\>@ this checkout reports to via
-- @gh repo view --json nameWithOwner@. Falls out as 'RepoCoords' so callers
-- never see a slash-separated string.
resolveRepoCoords :: IO (Either GhError RepoCoords)
resolveRepoCoords = do
  result <-
    runSubprocess
      "gh repo view"
      ghBin
      ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
      ""
  pure $ case result of
    Left e -> Left (GhSubprocess e)
    Right out -> case T.splitOn "/" (T.strip (T.pack out)) of
      [o, r] | not (T.null o), not (T.null r) -> Right (RepoCoords o r)
      _ -> Left (UnexpectedNameWithOwner out)
