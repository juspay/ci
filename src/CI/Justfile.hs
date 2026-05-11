{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Bindings for the @just@ CLI and its @--dump --dump-format json@ schema.
module CI.Justfile (Recipe, recipeDeps, fetchDump) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Exit (die)
import System.Process (readProcess)
import System.Which (staticWhich)

justBin :: FilePath
justBin = $(staticWhich "just")

newtype Dep = Dep {recipe :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Recipe = Recipe {dependencies :: [Dep]}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Dump = Dump {recipes :: Map.Map Text Recipe}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

recipeDeps :: Recipe -> [Text]
recipeDeps = map recipe . dependencies

fetchDump :: IO (Map.Map Text Recipe)
fetchDump = do
  raw <- TE.encodeUtf8 . T.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  Dump g <- either die pure (eitherDecodeStrict raw)
  pure g
