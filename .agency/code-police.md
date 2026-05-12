# Project rules for code-police

## comments-only-for-non-obvious

Default to writing no comments. Add one only when the **why** is non-obvious to a reader who can already see the code — a hidden constraint, a subtle invariant, a workaround for a specific bug, behavior that would surprise.

If removing the comment wouldn't confuse a future reader, don't write it.

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
- Mirror the JSON structure as Haskell records when each nested object carries distinct domain info; project in plain code afterwards. If a wire wrapper has no domain meaning of its own (typically a single-field object whose field _is_ the identifier, e.g. `{recipe: "name", ...}`), inline the extraction in `parseJSON` rather than introducing a noise newtype just to feed auto-derive.

_Anti-patterns_:

- Hand-writing `parseJSON = withObject "X" $ \o -> X <$> o .: "name" <*> o .: "age"` when the field names match. That's the literal contract Generic already gives you.
- Squashing nested objects at parse time *when those objects carry distinct domain info*. Mirror those; project in plain code afterwards.

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

## no-partial-functions

Don't reach for functions that can crash on a value of the correct type — `head`, `tail`, `init`, `last`, `fromJust`, `(!!)`, `read`, `Map.!`, `error`, `undefined` — when a total alternative exists. Non-exhaustive pattern matches fall under the same rule: every `case` covers every constructor (use `_` deliberately if you mean "I don't care").

_How to apply_:

- Need the first element? Pattern-match (`x : _`) or take `NonEmpty` and use `Data.List.NonEmpty.head`. Not `Data.List.head`.
- Looking up a `Map` key? `Map.lookup` (returns `Maybe`) or `Map.findWithDefault`. Not `Map.!`.
- Parsing text? `Text.Read.readMaybe`. Not `read`.
- `error`/`undefined` are reserved for invariants the type system can't express. Not for invalid input — that's an `Either`.

_Anti-patterns_:

- `head xs` where `xs` could be empty. Bake the non-emptiness into the type (`NonEmpty`) or pattern-match.
- `case x of Just y -> y` with no `Nothing` branch — `fromJust` in disguise.
- `error "shouldn't happen"` instead of refactoring so the impossibility is typed away (parameterize over the constraining type, or split the function).

## propagate-errors-via-either

Expected, recoverable failures flow through `Either e a` (or `m a` constrained by `MonadError e m`), not into `die`/`error`/`undefined`/`throwIO` deep in the call graph. The boundary — usually `main` or a request handler — is the only place that converts the `Either` to an exit code, log line, or HTTP response.

_How to apply_:

- A function that can fail for an *expected* reason (parse error, missing key, validation, schema mismatch) returns `Either e a` (pure) or `m a` for `MonadError e m` (stack).
- IO actions that can meaningfully fail return `IO (Either e a)` rather than throwing — caller decides whether to die or recover.
- Top-level entry points consume the `Either` once and translate to the appropriate side effect (`die`, `respond 4xx`, etc.).

_Anti-patterns_:

- `loadConfig :: IO Config` that `die`s on a parse failure. Caller can't see this can fail, can't recover, can't write a retry. Should be `IO (Either String Config)`.
- Catching an exception and rethrowing as `error "..."` — that's renaming a partial function, not fixing it.
- `fromRight (error "won't happen") result` — same partial function wearing a hat.

## prefer-newtype-over-string

Domain identifiers and values typed as `Text`/`String` should be wrapped in newtypes. A `Text` carrying a recipe name, user ID, URL, file path, semver string, etc. is a domain concept; the type system should know that. Without the newtype, the compiler can't distinguish a `Map Text Recipe` keyed by recipe name from one keyed by user ID, and signatures with several `Text` parameters become impossible to call correctly without re-reading docs.

_How to apply_:

- Wrap each domain concept in a positional `newtype RecipeName = RecipeName Text` (no field accessor). Derive `Eq`, `Ord`, `FromJSON`/`ToJSON`, and — for newtypes used as `Map` keys — `FromJSONKey`/`ToJSONKey` via `deriving newtype`. Runtime cost is zero.
- Derive `IsString` so callers construct from string literals: `"ci" :: RecipeName` with `OverloadedStrings`. That's the `fromString` half.
- Derive `Show` newtype-style for debugging and error messages — the underlying type's `Show` (e.g. Text's `"ci"`) is good enough. For richer display, define a `Display` typeclass; don't export an ad-hoc unwrapper.
- **Keep the constructor unexported.** External modules go through `IsString`/`Show`/`FromJSON` — never via a raw accessor. The whole point of the newtype is that the type-laundering API doesn't exist.
- Refactor signatures from `Text -> Map Text Foo -> Map Text [Text]` to `Name -> Map Name Foo -> Map Name [Name]` so the compiler catches swapped parameters.

_Anti-patterns_:

- Exporting `unRecipeName :: RecipeName -> Text` (or similar record accessors). That gives every caller a free pass to strip the type, defeating the newtype. `fromString`/`Show` cover construction and display; reach past them only inside the defining module.
- Pattern-matching on the constructor outside the defining module to extract the inner value. Same as the above with extra steps.
- `f :: Text -> Map Text Foo -> Either Text Bar` where the three `Text`s mean different things.
- A `String` filepath, URL, ID, or token threaded as a plain `String`. If it has a domain meaning, it has a newtype.
