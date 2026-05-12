{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings for the @just@ CLI and its @--dump --dump-format json@ schema.
module CI.Justfile (RecipeName, Recipe, recipeDeps, fetchDump) where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON, ToJSONKey, eitherDecodeStrict, withObject, (.:))
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

newtype Recipe = Recipe {recipeDeps :: [RecipeName]}

instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \o ->
    Recipe <$> (mapM (withObject "Dep" (.: "recipe")) =<< o .: "dependencies")

newtype Dump = Dump {recipes :: Map.Map RecipeName Recipe}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

fetchDump :: IO (Either String (Map.Map RecipeName Recipe))
fetchDump = do
  raw <- TE.encodeUtf8 . T.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  pure (recipes <$> eitherDecodeStrict raw)
