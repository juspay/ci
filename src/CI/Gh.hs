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
    Repo (..),

    -- * Operations
    GhError (..),
    viewRepo,
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

-- | A GitHub repository, matching @gh@'s vocabulary: an owner login plus a
-- repository name — the two halves of @nameWithOwner@. Resolved once from
-- @gh repo view@ and threaded into 'CI.CommitStatus.postConsumer'
-- alongside the 'CI.Git.Sha'.
data Repo = Repo {owner :: Text, name :: Text}
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

-- | Run @gh repo view --json nameWithOwner@ and split the result into a
-- typed 'Repo' so callers never see a slash-separated string.
viewRepo :: IO (Either GhError Repo)
viewRepo = do
  result <-
    runSubprocess
      "gh repo view"
      ghBin
      ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
      ""
  pure $ case result of
    Left e -> Left (GhSubprocess e)
    Right out -> case T.splitOn "/" (T.strip (T.pack out)) of
      [o, n] | not (T.null o), not (T.null n) -> Right (Repo o n)
      _ -> Left (UnexpectedNameWithOwner out)
