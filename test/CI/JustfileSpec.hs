{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "CI.Justfile"'s public surface. The focus is 'parseDump' —
-- the pure entry point that runs decode → flatten → qualify over a
-- @just --dump --dump-format json@ payload — and 'recipeCommand', the
-- one-line invocation builder. 'fetchDump' itself (subprocess + parse)
-- is covered end-to-end by the justfile's @run-check@ smoke test.
module CI.JustfileSpec (spec) where

import CI.Justfile (Dep (..), Recipe (..), RecipeName, parseDump, recipeCommand)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "recipeCommand" $ do
    it "emits --no-deps + the bare recipe name for a top-level recipe" $
      recipeCommand "default" `shouldSatisfy` T.isInfixOf " --no-deps default"
    it "emits --no-deps + the fully-qualified name for a submodule recipe" $
      recipeCommand "sub::entry" `shouldSatisfy` T.isInfixOf " --no-deps sub::entry"

  describe "parseDump" $ do
    context "top-level only" $ do
      it "decodes a single top-level recipe with no deps" $ do
        let recipes = decodeOrFail topLevelOnlyJson
        Map.keys recipes `shouldBe` ["solo"]
        depNames (recipes Map.! "solo") `shouldBe` []

    context "submodule recipes" $ do
      let recipes = decodeOrFail submoduleFixtureJson

      it "keys every recipe by its fully-qualified namepath" $
        Map.keys recipes
          `shouldMatchList` ["default", "sub::a", "sub::b", "sub::entry", "sub::shared"]

      it "leaves top-level recipe deps untouched" $
        depNames (recipes Map.! "default") `shouldBe` []

      it "qualifies an unqualified sibling dep with the owner's module path" $
        -- sub::a's source dep is bare 'shared'; should become 'sub::shared'
        depNames (recipes Map.! "sub::a") `shouldBe` ["sub::shared"]

      it "qualifies multiple sibling deps on the same recipe" $
        depNames (recipes Map.! "sub::entry") `shouldMatchList` ["sub::a", "sub::b"]

    context "already-qualified deps" $ do
      it "trusts a dep that already contains :: verbatim" $ do
        let recipes = decodeOrFail crossModuleDepJson
        depNames (recipes Map.! "default") `shouldBe` ["sub::leaf"]

    context "errors" $ do
      it "returns FetchParseError on malformed JSON" $
        case parseDump "{ not valid json" of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected Left on malformed JSON"

decodeOrFail :: BS.ByteString -> Map.Map RecipeName Recipe
decodeOrFail bs = case parseDump bs of
  Right m -> m
  Left e -> error ("parseDump failed: " <> show e)

depNames :: Recipe -> [RecipeName]
depNames r = map (\d -> d.recipe) r.dependencies

-- | One top-level recipe, no submodules. Smallest input that parseDump
-- can succeed on — exercises the no-flattening, no-qualifying path.
topLevelOnlyJson :: BS.ByteString
topLevelOnlyJson =
  "{\"recipes\":{\"solo\":{\"namepath\":\"solo\",\"dependencies\":[],\"parameters\":[],\"attributes\":[]}},\"modules\":{}}"

-- | A top-level recipe whose dep is already qualified to a submodule
-- recipe (e.g. a top-level @ci: sub::leaf@). Qualification must trust
-- the @::@ verbatim and not double-prefix.
crossModuleDepJson :: BS.ByteString
crossModuleDepJson =
  "{\"recipes\":{\"default\":{\"namepath\":\"default\",\"dependencies\":[{\"recipe\":\"sub::leaf\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[]}},\"modules\":{\"sub\":{\"recipes\":{\"leaf\":{\"namepath\":\"sub::leaf\",\"dependencies\":[],\"parameters\":[],\"attributes\":[]}},\"modules\":{}}}}"

-- | The shape the @test/fixtures/with-module@ justfile produces: a
-- bare top-level @default@ plus a @sub@ module with five recipes,
-- two of which (@a@, @b@) depend on a sibling (@shared@) and one
-- (@entry@) depends on both. Mirrors the json captured by running
-- @just --dump --dump-format json@ in that fixture directory.
submoduleFixtureJson :: BS.ByteString
submoduleFixtureJson =
  "{\"recipes\":{\"default\":{\"namepath\":\"default\",\"dependencies\":[],\"parameters\":[],\"attributes\":[]}},\
  \\"modules\":{\"sub\":{\"recipes\":{\
  \\"a\":{\"namepath\":\"sub::a\",\"dependencies\":[{\"recipe\":\"shared\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[]},\
  \\"b\":{\"namepath\":\"sub::b\",\"dependencies\":[{\"recipe\":\"shared\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[]},\
  \\"entry\":{\"namepath\":\"sub::entry\",\"dependencies\":[{\"recipe\":\"a\",\"arguments\":[]},{\"recipe\":\"b\",\"arguments\":[]}],\"parameters\":[],\"attributes\":[{\"metadata\":[\"ci\"]}]},\
  \\"shared\":{\"namepath\":\"sub::shared\",\"dependencies\":[],\"parameters\":[],\"attributes\":[]}\
  \},\"modules\":{}}}}"
