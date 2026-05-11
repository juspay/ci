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
