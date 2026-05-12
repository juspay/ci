# Project rules for code-police

## document-symbols

Top-level symbols carry Haddock comments. The bar differs by visibility:

- **Every exported symbol** is documented — no exceptions. Multi-concern export lists are organized into named groups so the public API reads like a table of contents.
- **Internal symbols** are documented when their purpose isn't obvious from name and type alone. A type that mirrors an external JSON shape, a TH-spliced binary path, a record whose fields encode a non-trivial protocol — those want a Haddock. A trivial alias or one-line helper doesn't.

Only export a symbol if something outside the module imports it. Dead exports are dead code with an extra step.

_How to apply_:

- Above each exported declaration (function, type, instance, etc.), write a `-- |` Haddock describing what it is and why a caller would use it. Exception: a single-function module whose module-level Haddock already describes that function (typical `Main`) doesn't need the per-function repeat.
- For internal symbols, ask: would a reader who didn't write this know what this is for from name and type alone? If not, write the Haddock.
- When a module exports more than a couple of names across distinct concerns, organize the export list with Haddock group headings (`-- * Group name`).
- Don't export a name unless something outside the module imports it. Grep the codebase; if nobody imports `foo`, drop it from the export list (and probably the binding — see `no-dead-code`).

_Anti-patterns_:

- An exported declaration with no Haddock above its type signature.
- An internal data type whose purpose requires reading consumer code to understand.
- A multi-concern export list with no group headings.
- Exports that nothing imports.

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

Default to Generic-based auto-derivation (`deriving stock Generic` + `deriving anyclass FromJSON`/`ToJSON`). Hand-write `parseJSON`/`toJSON` only when auto-derivation genuinely can't express the mapping.

_How to apply_:

- **Name Haskell record fields to match the JSON keys.** aeson Generic ignores unknown fields, so missing keys aren't a parse error. Each omission is a deliberate design choice though — verify the field has no domain meaning your consumer needs before deciding it's ignorable. "Ignore" should mean "I read the source schema and this is runtime metadata", not "I haven't looked at what this field is for".
- Mirror the wire structure as records; write a `data` → `data` projection function for any consumer that wants flattened data. The parser stays generic.
- Only if the JSON keys can't match the Haskell field names (reserved words, casing constraints), use `genericParseJSON` with an `Options { fieldLabelModifier = ... }` modifier. Still derives, just with a transform.

_Anti-patterns_:

- Hand-writing `parseJSON = withObject "X" $ \o -> X <$> o .: "name" <*> o .: "age"` when the field names match the JSON. That's the literal contract Generic already gives you.
- Renaming Haskell fields away from the JSON keys "for aesthetics" and then needing `fieldLabelModifier`. Match the wire; rename in the projection layer if anyone needs it.

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

## structured-errors

Error values are structured types, not strings. A function that can fail in distinct ways exposes those failure modes as constructors so callers can pattern-match, log structured fields, or recover programmatically. `String`/`Text` errors collapse all failure paths into one opaque blob — readable in CLI output, useless to any caller that wants to do more than dump and die.

_How to apply_:

- Define a sum type per error domain: `data FetchError = FetchParseError String | NetworkError IOException | ...`. Functions return `Either MyError a` (or `m a` for `MonadError MyError m`).
- The **display layer** — where the error reaches a human — owns the formatting: a `displayError :: MyError -> Text` function in the same module as the error type. The boundary (`main`, a handler) calls it once.
- If the value an upstream library hands you is genuinely just a `String` (e.g. aeson's `eitherDecodeStrict` returns `Either String a`), wrap it in a single-constructor type: `data FetchError = FetchParseError String`. The string survives at runtime; the type system now distinguishes "this is a fetch error" from other error kinds.

_Anti-patterns_:

- `Either String a` or `IO (Either String a)` as a function's return type. Callers can't discriminate failure modes.
- Building a user-facing message string inline at the failure site (`Left ("recipe " <> show k <> " not found")`). The structured error should carry `k`; the display function formats it.
- Catching a structured error and re-throwing as a `String` — same bug, one layer deeper.

## use-record-dot

Enable `OverloadedRecordDot` for `r.field` syntax in modules that define or read records. Use plain `r.field` for reads; don't write one-line wrapper functions whose entire body is a single field access, and don't reach for sectioned-functor forms when a plain expression works.

Related extensions to enable per module as needed:

- `NoFieldSelectors` — suppress the auto-generated `field :: Record -> Field` selector functions so dot access is the only path. Keeps the namespace clean.
- `DuplicateRecordFields` — allow multiple records in the same module to share field names. Dot syntax disambiguates by the value's type.
- `RecordWildCards` — `Foo {..}` brings all fields into scope at construction or pattern-match. Useful for records with many fields.

_How to apply_:

- Use `r.field` for direct reads. Pattern-match (or `RecordWildCards`) when destructuring; record-update syntax (`r { f = v }`) for updates.
- For mapping field access over a structure, use a list comprehension (`[d.recipe | d <- r.deps]`) or an explicit lambda. Plain syntax beats clever syntax.
- Inline trivial accesses at the call site. A function whose body is a single field selection is the auto-generated selector under another name.

_Anti-patterns_:

- A module exporting `getFoo :: Bar -> Foo` whose body is `\b -> b.foo`. That's the selector renamed.
- `(.field) <$> r.items` — sectioned-functor composition. Prefer `[i.field | i <- r.items]` (plain dot accesses, no sections) or a `\i -> i.field` lambda.
- Reaching for `foo bar` (function-call style) when `bar.foo` works in scope.

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
