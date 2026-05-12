{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Entry point: emit a @process-compose@ YAML config for the @just@ recipe
-- subgraph reachable from the @[metadata(\"entrypoint\")]@ recipe.
module Main where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Graph (reachableSubgraph)
import CI.Justfile (Attribute (..), Recipe (..), RecipeName, fetchDump)
import CI.ProcessCompose (toProcessCompose)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)
import qualified Data.Yaml as Y
import System.Exit (die)

main :: IO ()
main = do
  recipes <- either (die . T.unpack . display) pure =<< fetchDump
  root <- either (die . T.unpack . display) pure $ findEntrypoint recipes
  subgraph <- either (die . T.unpack . display) pure $ reachableSubgraph root recipes
  let reachable = Map.restrictKeys recipes (G.vertexSet subgraph)
  pc <- either (die . T.unpack . display) pure $ toProcessCompose reachable
  BS.putStr $ Y.encode pc

-- | Failures from 'findEntrypoint'.
data EntrypointError
  = -- | No recipe carries an @entrypoint@ metadata tag.
    NoEntrypoint
  | -- | More than one recipe carries one; the runner refuses to guess.
    MultipleEntrypoints [RecipeName]

instance Display EntrypointError where
  displayBuilder NoEntrypoint =
    "no recipe is tagged [metadata(\"entrypoint\")]; mark exactly one recipe as the CI entry point"
  displayBuilder (MultipleEntrypoints rs) =
    "multiple recipes are tagged [metadata(\"entrypoint\")]: "
      <> displayBuilder (T.intercalate ", " (display <$> rs))

-- | Find the single recipe tagged with @[metadata(\"entrypoint\")]@. Refuses
-- to silently pick one when more than one is tagged.
findEntrypoint :: Map.Map RecipeName Recipe -> Either EntrypointError RecipeName
findEntrypoint recipes =
  case [name | (name, r) <- Map.toList recipes, any isEntrypoint r.attributes] of
    [] -> Left NoEntrypoint
    [name] -> Right name
    xs -> Left (MultipleEntrypoints xs)
  where
    isEntrypoint = \case
      Metadata ms -> "entrypoint" `elem` ms
      _ -> False
