{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings for the @just@ CLI and its @--dump --dump-format json@ schema.
module CI.Justfile
  ( -- * Schema
    RecipeName,
    Recipe,
    recipeDeps,

    -- * Fetching
    fetchDump,
  )
where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey, Options (..), ToJSON, ToJSONKey, defaultOptions, eitherDecodeStrict, genericParseJSON)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Process (readProcess)
import System.Which (staticWhich)

-- | Absolute path to the @just@ binary, baked in at compile time via Nix.
justBin :: FilePath
justBin = $(staticWhich "just")

-- | The identifier of a recipe, as it appears in a justfile and as a key in @just --dump@'s @recipes@ map.
newtype RecipeName = RecipeName Text
  deriving newtype (Show, Eq, Ord, IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

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

-- | A parsed recipe: its declared dependencies and formal parameters.
data Recipe = Recipe
  { dependencies :: [Dep],
    parameters :: [Parameter]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | The top-level @just --dump@ object. We only model the @recipes@ field; aeson ignores the rest.
newtype Dump = Dump {recipes :: Map.Map RecipeName Recipe}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Names of the recipes this recipe directly depends on. Drops the @arguments@ at each call site.
recipeDeps :: Recipe -> [RecipeName]
recipeDeps = map recipe . dependencies

-- | Invoke @just --dump --dump-format json@ and decode the @recipes@ map. Returns 'Left' with the underlying decode error on parse failure.
fetchDump :: IO (Either String (Map.Map RecipeName Recipe))
fetchDump = do
  raw <- TE.encodeUtf8 . T.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  pure (recipes <$> eitherDecodeStrict raw)
