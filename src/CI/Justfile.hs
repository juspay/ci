{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings for the @just@ CLI and its @--dump --dump-format json@ schema.
module CI.Justfile (RecipeName, Recipe, recipeDeps, fetchDump) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, eitherDecodeStrict)
import qualified Data.Map.Strict as Map
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Process (readProcess)
import System.Which (staticWhich)

justBin :: FilePath
justBin = $(staticWhich "just")

newtype RecipeName = RecipeName Text
  deriving newtype (Show, Eq, Ord, IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

newtype Dep = Dep {recipe :: RecipeName}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Recipe = Recipe {dependencies :: [Dep]}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Dump = Dump {recipes :: Map.Map RecipeName Recipe}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

recipeDeps :: Recipe -> [RecipeName]
recipeDeps = map recipe . dependencies

fetchDump :: IO (Either String (Map.Map RecipeName Recipe))
fetchDump = do
  raw <- TE.encodeUtf8 . T.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  pure (recipes <$> eitherDecodeStrict raw)
