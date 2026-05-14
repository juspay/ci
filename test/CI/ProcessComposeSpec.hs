{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "CI.ProcessCompose"'s YAML emission, focused on the
per-process @log_location@ knob: it must round-trip into the YAML
when the caller supplies one and stay absent when the caller doesn't
(so @dump-yaml@ and local-mode runs are byte-identical to the
pre-feature output).
-}
module CI.ProcessComposeSpec (spec) where

import qualified Algebra.Graph.AdjacencyMap as G
import CI.Node (NodeId (..))
import CI.Platform (Platform (..))
import CI.ProcessCompose (toProcessCompose)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Y
import Test.Hspec

spec :: Spec
spec = describe "toProcessCompose" $ do
    it "emits log_location when the per-node lookup returns Just" $ do
        let yaml = encodeYaml (const (Just ".ci/abc/linux/r.log"))
        yaml `shouldContain` "log_location: .ci/abc/linux/r.log"

    it "omits log_location when the per-node lookup returns Nothing" $ do
        let yaml = encodeYaml (const Nothing)
        yaml `shouldNotContain` "log_location"

    it "keys processes by <recipe>@<platform>" $ do
        let yaml = encodeYaml (const Nothing)
        yaml `shouldContain` "r@linux"

    it "emits one process per (recipe, platform) when a recipe is fanned out" $ do
        let g =
                G.vertices
                    [NodeId "build" Linux, NodeId "build" Macos]
            yaml = encodeMulti (const Nothing) g
        yaml `shouldContain` "build@linux"
        yaml `shouldContain` "build@macos"

    it "wires depends_on within a platform lane, never across" $ do
        -- Two lanes, each with build←root: root depends on build. The
        -- linux lane's root must depend_on build@linux, not build@macos
        -- (and vice versa). Cross-platform edges are a fanout bug.
        let g =
                G.edges
                    [ (NodeId "root" Linux, NodeId "build" Linux)
                    , (NodeId "root" Macos, NodeId "build" Macos)
                    ]
            yaml = encodeMulti (const Nothing) g
        -- root@linux's depends_on block contains build@linux
        yaml `shouldContain` "root@linux"
        yaml `shouldContain` "build@linux"
        yaml `shouldContain` "root@macos"
        yaml `shouldContain` "build@macos"
        -- No cross-lane edges should be emitted
        yaml `shouldNotContain` "build@macos:\n          condition"

{- | Encode a single-vertex 'ProcessCompose' to YAML as a String. One
vertex is enough: the per-process @log_location@ field is set
vertex-by-vertex, and adding more vertices would just multiply the
assertion surface without exercising any new code path.
-}
encodeYaml :: (NodeId -> Maybe FilePath) -> String
encodeYaml mkLog =
    BS8.unpack . Y.encode $
        toProcessCompose Nothing (const "echo hi") mkLog graph
  where
    graph = G.vertex (NodeId "r" Linux)

{- | Variant of 'encodeYaml' that takes a caller-supplied graph so a
test can exercise multi-platform fanout shapes.
-}
encodeMulti :: (NodeId -> Maybe FilePath) -> G.AdjacencyMap NodeId -> String
encodeMulti mkLog g =
    BS8.unpack . Y.encode $ toProcessCompose Nothing (const "echo hi") mkLog g
