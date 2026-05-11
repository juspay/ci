# Project rules for code-police

## comments-only-for-non-obvious

Default to writing no comments. Add one only when the **why** is non-obvious to a reader who can already see the code — a hidden constraint, a subtle invariant, a workaround for a specific bug, behavior that would surprise.

If removing the comment wouldn't confuse a future reader, don't write it.

_Anti-patterns_:

- **Restating the code**: `-- BFS over the recipe graph` next to a function literally named `bfs`. Well-named identifiers already explain *what*; the comment adds nothing.
- **Narrating the current change**: `-- added for the just-graph PR`, `-- used by the run-check flow`, `-- fix for issue #123`. Belongs in the commit/PR, not the source — those facts rot as the code moves on.
- **TODOs without an issue link**: `-- TODO: handle this better`. Either fix it now or file an issue and reference it.

## one-module-one-concern

Each `.hs` module owns a single concern. Don't pile unrelated types, functions, and IO into one module just because they're new — group code by what changes together, not by what's convenient to type into one file.

_How to apply_:

- Ask "what's the one sentence that describes this module?" If you can't say it without "and", split.
- Generic algorithms (reachability, encoding, formatting) live in their own module so they can be reused or replaced without churn in unrelated code.
- IO and shell-outs to a specific external tool live in that tool's module — not the entry point.

_Anti-patterns_:

- One file holding: schema types + FromJSON instances + Template-Haskell binary lookup + BFS algorithm + JSON re-emitter + `main`. Each is a different rate of change.
- A `Utils.hs` / `Common.hs` that grows by accretion. If you can't name its single concern, it's a dumping ground.

## prefer-aeson-auto-derive

Default to Generic-based auto-derivation (`deriving stock Generic` + `deriving anyclass FromJSON`/`ToJSON`) for JSON parsers. Hand-write `parseJSON`/`toJSON` only when auto-derivation genuinely can't express the mapping — and even then, reach for `genericParseJSON` with an `Options { fieldLabelModifier = ... }` before falling back to `withObject` + `(.:)`.

_How to apply_:

- Use `deriving stock Generic` + `deriving anyclass FromJSON` (or `ToJSON`) and let aeson read the field names directly off the record.
- If the JSON keys differ from the Haskell field names, use `genericParseJSON` with an `Options { fieldLabelModifier = ... }` modifier. Still derives — just with a transform.
- Mirror the JSON structure as Haskell records; if the on-the-wire shape is nested, mirror that, then write a `data` → `data` projection function. The parser stays generic; the projection stays explicit.

_Anti-patterns_:

- Hand-writing `parseJSON = withObject "X" $ \o -> X <$> o .: "name" <*> o .: "age"` when the field names match. That's the literal contract Generic already gives you.
- Squashing nested objects at parse time. The parser shouldn't do projection work; it should mirror the wire format, with a separate function doing the unwrap.

## module-needs-description

Every `.hs` module must carry a top-level Haddock comment naming what the module is for — a single sentence is fine. This is documentation of the module's concern, not commentary on the code (the `comments-only-for-non-obvious` rule still applies inside the body).

_How to apply_:

- Place `-- | <one-line description>` immediately above the `module` declaration.
- For multi-line descriptions, use a `{-| ... -}` block.
- The sentence should be the same answer you'd give to `one-module-one-concern` — "this module is for X". If you can't write that sentence, the module is doing too much.

_Anti-patterns_:

- Restating the module name: `-- | The Main module.`
- Listing exports: `-- | Exports foo, bar, baz.`
- Narrating the diff: `-- | Added in PR #2 for the just-graph feature.`
