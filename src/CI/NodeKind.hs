{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Which 'CI.Node.NodeId's are internal plumbing vs. user-facing
recipes — a *reporting-visibility* policy, kept separate from
'CI.Node' (which owns identity representation).

Today there's exactly one internal kind — the synthetic
@_ci-setup\@\<platform\>@ node that ships the bundle once per
remote per run. Its existence is a fanout optimization, not
something the PR author asked for, so the GitHub commit-status
posts and the local verdict summary both filter it out. The
predicate lives here so a future second internal kind (teardown,
pre-flight, etc.) is added in one place and every consumer
('CI.CommitStatus', 'CI.Verdict') stays in lockstep.

'CI.Pipeline' (which builds setup nodes into the graph) and
'CI.Transport' (which formerly read the predicate inside
'commandFor') also import 'setupRecipe' from here — keeping the
reserved-name constant collocated with the predicate that tests
for it.
-}
module CI.NodeKind (
    setupRecipe,
    isSetupNode,
)
where

import CI.Justfile (RecipeName)
import CI.Node (NodeId (..))

{- | The reserved recipe name for the per-platform setup node that
ships the @just@ derivation and the bundled HEAD to a remote.
Leading underscore signals "internal, not a user recipe"; the
same prefix convention kolu uses for private lanes.
-}
setupRecipe :: RecipeName
setupRecipe = "_ci-setup"

{- | Whether a 'NodeId' is the synthetic setup node for its platform.
Used by 'CI.CommitStatus' to skip GH signoff for setup nodes,
and by 'CI.Verdict' to omit them from the summary — those are
internal plumbing, not user recipes the PR cares about.
-}
isSetupNode :: NodeId -> Bool
isSetupNode n = n.recipe == setupRecipe
