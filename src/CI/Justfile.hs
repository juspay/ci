{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Bindings for the @just@ CLI and its @--dump --dump-format json@ schema.
module CI.Justfile
  ( -- * Schema
    RecipeName,
    Recipe (..),
    Dep (..),

    -- * Fetching
    FetchError (..),
    fetchDump,
  )
where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey, Options (..), ToJSON, ToJSONKey, Value, defaultOptions, eitherDecodeStrict, genericParseJSON)
import Data.Bifunctor (bimap)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)

-- | Absolute path to the @just@ binary, baked in at compile time via Nix.
justBin :: FilePath
justBin = $(staticWhich "just")

-- | The identifier of a recipe, as it appears in a justfile and as a key in @just --dump@'s @recipes@ map.
newtype RecipeName = RecipeName Text
  deriving newtype (Show, Eq, Ord, IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance Display RecipeName where
  displayBuilder (RecipeName t) = displayBuilder t

-- | One entry in a recipe's @dependencies@ array: the dep's target name plus any arguments passed at this call site (only non-empty when the target is parameterized).
data Dep = Dep
  { recipe :: RecipeName,
    arguments :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | One entry in a recipe's @parameters@ array: a formal parameter the recipe declares. Mirrors the nine fields @just@ emits per parameter.
data Parameter = Parameter
  { name :: Text,
    default_ :: Maybe Text,
    export :: Bool,
    help :: Maybe Text,
    kind :: Text,
    long :: Maybe Text,
    pattern :: Maybe Text,
    short :: Maybe Text,
    value :: Maybe Text
  }
  deriving stock (Generic)

-- Custom because @default@ is a Haskell keyword; the field is @default_@ here
-- and stripped back to @default@ for the JSON.
instance FromJSON Parameter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropWhileEnd (== '_')}

-- | A parsed recipe: its declared dependencies, formal parameters, and recipe-level attributes. The @attributes@ array is heterogeneous in @just@'s JSON — flag attributes like @parallel@ serialize as bare strings, parameterized ones like @group("ci")@ serialize as single-key objects — so it's passed through as raw 'Value's. Interpretation belongs to consumers, not this schema mirror.
data Recipe = Recipe
  { dependencies :: [Dep],
    parameters :: [Parameter],
    attributes :: [Value]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | The top-level @just --dump@ object. We only model the @recipes@ field; aeson ignores the rest.
newtype Dump = Dump {recipes :: Map.Map RecipeName Recipe}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Failures from 'fetchDump'.
data FetchError
  = -- | The @just@ subprocess exited non-zero. Carries the exit code and the captured stderr.
    FetchProcessError Int String
  | -- | The @just@ subprocess succeeded but its JSON output didn't decode. Carries aeson's underlying message.
    FetchParseError String
  deriving stock (Show)

instance Display FetchError where
  displayBuilder (FetchProcessError n stderr) =
    "just exited with code " <> displayBuilder n <> ": " <> displayBuilder (T.pack stderr)
  displayBuilder (FetchParseError msg) =
    "failed to decode just dump: " <> displayBuilder (T.pack msg)

-- | Invoke @just --dump --dump-format json@ and decode the @recipes@ map. Process failures and JSON parse failures are both surfaced as 'FetchError'; no exception escapes.
fetchDump :: IO (Either FetchError (Map.Map RecipeName Recipe))
fetchDump = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode justBin ["--dump", "--dump-format", "json"] ""
  pure $ case exitCode of
    ExitFailure n -> Left $ FetchProcessError n stderr
    ExitSuccess -> bimap FetchParseError (\d -> d.recipes) (eitherDecodeStrict @Dump (TE.encodeUtf8 (T.pack stdout)))
