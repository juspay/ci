{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A subprocess-with-exit-code helper. Wraps non-zero exits as a structured
-- 'SubprocessError' so every shell-out in the project routes failures
-- through the same type and Display instance, instead of each module
-- repeating @case ec of ExitFailure n -> Left (FooFailed n err)@.
module CI.Subprocess
  ( SubprocessError (..),
    runSubprocess,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | A subprocess that exited non-zero. 'description' is a human-readable
-- name for the invocation (e.g. @"git rev-parse HEAD"@) used in error
-- display; 'code' is the exit code; 'stderr' is captured standard error.
data SubprocessError = SubprocessError
  { description :: Text,
    code :: Int,
    stderr :: String
  }
  deriving stock (Show)

instance Display SubprocessError where
  displayBuilder e =
    displayBuilder e.description
      <> " failed ("
      <> displayBuilder e.code
      <> "): "
      <> displayBuilder (T.pack e.stderr)

-- | Run a subprocess, returning the captured stdout on success or a
-- 'SubprocessError' on non-zero exit. @description@ identifies the
-- invocation in error messages; @bin@ is the absolute path to the binary
-- (typically a @\$(staticWhich ...)@ splice); @args@ are the invocation
-- args; @stdin@ is piped to the subprocess.
runSubprocess :: Text -> FilePath -> [String] -> String -> IO (Either SubprocessError String)
runSubprocess description bin args input = do
  (ec, out, err) <- readProcessWithExitCode bin args input
  pure $ case ec of
    ExitSuccess -> Right out
    ExitFailure n -> Left (SubprocessError description n err)
