{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | CI-entrypoint discovery policy.
--
-- This lives in its own module because the axis of change is independent of
-- @just@'s wire format (which 'CI.Justfile' encapsulates): the policy is
-- which recipe in a decoded recipe map is treated as the CI root, and that
-- can evolve (e.g. CLI override, config file, well-known name) without any
-- corresponding change to how @just --dump@ output is parsed.
module CI.Entrypoint
  ( EntrypointError,
    findEntrypoint,
  )
where

import CI.Justfile (Attribute (..), Recipe (..), RecipeName)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)

-- | Failures from 'findEntrypoint'.
data EntrypointError
  = -- | No recipe carries an @entrypoint@ metadata tag.
    NoEntrypoint
  | -- | More than one recipe carries one; the runner refuses to guess.
    MultipleEntrypoints [RecipeName]
  deriving stock (Show)

instance Display EntrypointError where
  displayBuilder NoEntrypoint =
    "no recipe is tagged [metadata(\"entrypoint\")]; mark exactly one recipe as the CI entry point"
  displayBuilder (MultipleEntrypoints rs) =
    "multiple recipes are tagged [metadata(\"entrypoint\")]: "
      <> displayBuilder (T.intercalate ", " (display <$> rs))

-- | Find the single recipe tagged with @[metadata(\"entrypoint\")]@. Refuses to silently pick one when more than one is tagged.
findEntrypoint :: Map.Map RecipeName Recipe -> Either EntrypointError RecipeName
findEntrypoint recipes =
  case [name | (name, r) <- Map.toList recipes, any isEntrypoint r.attributes] of
    [] -> Left NoEntrypoint
    [name] -> Right name
    xs -> Left (MultipleEntrypoints xs)
  where
    isEntrypoint (Metadata ms) = "entrypoint" `elem` ms
    isEntrypoint _ = False
