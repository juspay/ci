{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The runner DAG's node identity: a recipe paired with the platform
it's targeted at. Process-compose's wire vocabulary keys processes
by a single 'Data.Text.Text', so 'NodeId' has a canonical textual
form (via 'Display') and a parser ('parseNodeId') for round-tripping
a @ProcessState.name@ back into a typed value.

The @\<recipe\>\@\<platform\>@ separator is chosen because recipe
FQNs use @::@ (so collisions are impossible) and @\@@ needs no
shell quoting in any consumer. Kolu uses the same convention for
its GitHub commit-status contexts.
-}
module CI.Node (
    NodeId (..),
    parseNodeId,
)
where

import CI.Justfile (RecipeName, recipeNameFromText)
import CI.Platform (Platform, parsePlatform)
import Data.Aeson (ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (Display (..), display)

{- | The runner DAG node: which recipe, on which target platform.
Lifted out of a bare 'RecipeName' once the runner became
multi-platform — every downstream map key, log path, and
commit-status context that used to consume a 'RecipeName' now
consumes a 'NodeId'.
-}
data NodeId = NodeId
    { recipe :: RecipeName
    , platform :: Platform
    }
    deriving stock (Show, Eq, Ord)

{- | The canonical wire form: @\<recipe\>\@\<platform\>@. This is the
string process-compose sees as a process name and the context name
on the GitHub commit status. Every consumer renders through this
one instance.
-}
instance Display NodeId where
    displayBuilder n = displayBuilder n.recipe <> "@" <> displayBuilder n.platform

{- | YAML/JSON key encoding for the process-compose @processes@ map.
Routes through 'Display' so the wire form ('@'-separated) is the
*only* serialization; no parallel JSON-specific shape can drift.
-}
instance ToJSON NodeId where
    toJSON = toJSON . display

instance ToJSONKey NodeId where
    toJSONKey = toJSONKeyText display

{- | Inverse of 'display'. The wire-side observer
('CI.ProcessCompose.Events.subscribeStates') hands us a raw
'Text' and we recover the typed value here. Splits on the *last*
@\@@ so a recipe FQN containing no @\@@ (the usual case) and the
platform suffix are unambiguous; 'Nothing' on any unparseable
input.

Failure mode is silent-drop at the call site (see
'CI.Verdict.recordOutcome'): an unknown wire name means the run
emitted a process we didn't schedule, which is a contract
violation we surface elsewhere rather than crash on here.
-}
parseNodeId :: Text -> Maybe NodeId
parseNodeId t = case T.breakOnEnd "@" t of
    ("", _) -> Nothing
    (prefixWithSep, platformText) -> do
        let recipeText = T.dropEnd 1 prefixWithSep
        p <- parsePlatform platformText
        if T.null recipeText
            then Nothing
            else Just (NodeId (recipeNameFromText recipeText) p)
