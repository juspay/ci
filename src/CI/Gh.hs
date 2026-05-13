{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | GitHub CLI (@gh@) operations. Owns the binary path, every shell-out to
-- @gh@ in the project, and the GitHub API details those shell-outs encode:
-- endpoint URLs, wire values for typed enums, form-field names. Callers
-- reach for typed operations ('viewRepo', 'postCommitStatus') with typed
-- arguments — no raw endpoints, no @-f key=value@ pairs.
module CI.Gh
  ( -- * Values
    Owner,
    RepoName,
    Repo,
    CommitStatus (..),
    Context,
    CommitStatusPost (..),

    -- * Operations
    GhError (..),
    viewRepo,
    contextFrom,
    postCommitStatus,
  )
where

import CI.Git (Sha)
import CI.Subprocess (SubprocessError, runSubprocess)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import System.Which (staticWhich)

-- | Absolute path to the @gh@ binary, baked in at compile time via Nix.
-- Not exported: every gh shell-out in the project goes through one of the
-- typed operations below.
ghBin :: FilePath
ghBin = $(staticWhich "gh")

-- | A GitHub owner login (user or org). Opaque; minted only by 'viewRepo'.
newtype Owner = Owner Text
  deriving stock (Show, Eq)
  deriving newtype (Display)

-- | A GitHub repository name (the @name@ half of @nameWithOwner@). Opaque;
-- minted only by 'viewRepo'.
newtype RepoName = RepoName Text
  deriving stock (Show, Eq)
  deriving newtype (Display)

-- | A GitHub repository, matching @gh@'s vocabulary: an owner login plus a
-- repository name — the two halves of @nameWithOwner@. Resolved once from
-- @gh repo view@ and threaded into 'CI.CommitStatus.postConsumer'
-- alongside the 'CI.Git.Sha'.
data Repo = Repo {owner :: Owner, name :: RepoName}
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
      [o, n] | not (T.null o), not (T.null n) -> Right (Repo (Owner o) (RepoName n))
      _ -> Left (UnexpectedNameWithOwner out)

-- | The four GitHub-defined commit-status states. See
-- <https://docs.github.com/en/rest/commits/statuses>. The 'Display'
-- instance emits the lowercase wire form GitHub accepts.
data CommitStatus = Pending | Success | Failure | Error
  deriving stock (Show, Eq)

instance Display CommitStatus where
  displayBuilder Pending = "pending"
  displayBuilder Success = "success"
  displayBuilder Failure = "failure"
  displayBuilder Error = "error"

-- | A status-check context: the unique label that groups posts of the same
-- check on a PR. Opaque; minted only via 'contextFrom'. GitHub treats the
-- value itself as free-form text — the @ci/\<recipe\>@ naming convention
-- is CI policy, owned by "CI.CommitStatus".
newtype Context = Context Text
  deriving stock (Show)
  deriving newtype (Display)

-- | The smart constructor for 'Context'. The value is opaque to GitHub, so
-- this is just @Context@ today — the named entry point makes every
-- minting site searchable and gives one place to add validation later
-- without touching call sites.
contextFrom :: Text -> Context
contextFrom = Context

-- | The fields the @Create-a-commit-status@ endpoint expects, grouped so
-- callers don't pass three positional values. @description@ is free-form
-- caller-supplied prose.
data CommitStatusPost = CommitStatusPost
  { state :: CommitStatus,
    context :: Context,
    description :: Text
  }

-- | POST @\/repos\/{owner}\/{repo}\/statuses\/{sha}@ with the given status
-- post. The endpoint URL and wire encoding of 'CommitStatus' are gh-API
-- details owned here; the caller passes only typed values.
postCommitStatus :: Repo -> Sha -> CommitStatusPost -> IO (Either SubprocessError ())
postCommitStatus repo sha post = do
  let endpoint = "/repos/" <> display repo.owner <> "/" <> display repo.name <> "/statuses/" <> display sha
      args =
        [ "api",
          "-X",
          "POST",
          T.unpack endpoint,
          "-f",
          "state=" <> T.unpack (display post.state),
          "-f",
          "context=" <> T.unpack (display post.context),
          "-f",
          "description=" <> T.unpack post.description
        ]
  result <- runSubprocess ("gh api POST " <> endpoint) ghBin args ""
  pure (() <$ result)
