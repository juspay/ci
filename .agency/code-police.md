# Project rules for code-police

## comments-only-for-non-obvious

Default to writing no comments. Add one only when the **why** is non-obvious to a reader who can already see the code — a hidden constraint, a subtle invariant, a workaround for a specific bug, behavior that would surprise.

If removing the comment wouldn't confuse a future reader, don't write it.

_Anti-patterns_:

- **Restating the code**: `-- BFS over the recipe graph` next to a function literally named `bfs`. Well-named identifiers already explain *what*; the comment adds nothing.
- **Narrating the current change**: `-- added for the just-graph PR`, `-- used by the run-check flow`, `-- fix for issue #123`. Belongs in the commit/PR, not the source — those facts rot as the code moves on.
- **TODOs without an issue link**: `-- TODO: handle this better`. Either fix it now or file an issue and reference it.
