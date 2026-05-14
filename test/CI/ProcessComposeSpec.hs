{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.ProcessCompose"'s YAML emission, focused on the
-- per-process @log_location@ knob: it must round-trip into the YAML
-- when the caller supplies one and stay absent when the caller doesn't
-- (so @dump-yaml@ and local-mode runs are byte-identical to the
-- pre-feature output).
module CI.ProcessComposeSpec (spec) where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Justfile (RecipeName)
import CI.ProcessCompose (toProcessCompose)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import Test.Hspec

spec :: Spec
spec = describe "toProcessCompose" $ do
  it "emits log_location when the per-recipe lookup returns Just" $ do
    let yaml = encodeYaml (const (Just ".ci/abc/r.log"))
    yaml `shouldContain` "log_location: .ci/abc/r.log"

  it "omits log_location when the per-recipe lookup returns Nothing" $ do
    let yaml = encodeYaml (const Nothing)
    yaml `shouldNotContain` "log_location"

-- | Encode a single-vertex 'ProcessCompose' to YAML as a String. One
-- vertex is enough: the per-process @log_location@ field is set
-- vertex-by-vertex, and adding more vertices would just multiply the
-- assertion surface without exercising any new code path.
encodeYaml :: (RecipeName -> Maybe FilePath) -> String
encodeYaml mkLog =
  T.unpack . TE.decodeUtf8 . BS.copy $
    Y.encode (toProcessCompose Nothing (const "echo hi") mkLog graph)
  where
    graph = G.vertex "r"
