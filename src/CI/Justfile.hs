{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CI.Justfile (Recipe (..), fetchDump) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Exit (die)
import System.Process (readProcess)
import System.Which (staticWhich)

justBin :: FilePath
justBin = $(staticWhich "just")

newtype Recipe = Recipe {recipeDeps :: [Text]}

instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \o ->
    Recipe <$> (mapM (withObject "Dep" (.: "recipe")) =<< o .: "dependencies")

newtype Dump = Dump (Map.Map Text Recipe)

instance FromJSON Dump where
  parseJSON = withObject "Dump" $ \o -> Dump <$> o .: "recipes"

fetchDump :: IO (Map.Map Text Recipe)
fetchDump = do
  raw <- TE.encodeUtf8 . T.pack <$> readProcess justBin ["--dump", "--dump-format", "json"] ""
  Dump g <- either die pure (eitherDecodeStrict raw)
  pure g
