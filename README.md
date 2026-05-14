# ci

A Haskell pipeline runner: translates a `just` recipe graph into a [process-compose](https://f1bonacc1.github.io/process-compose/) DAG and drives it. Sibling recipes keep running after one fails; the final exit code is derived from a per-recipe outcome map that the central observer accumulates, not from process-compose's own exit. In strict mode, posts per-recipe GitHub commit statuses live as the pipeline runs.

The pipeline root is the recipe annotated `[metadata("ci")]` — exactly one across the justfile and its submodules (zero or multiple is a startup error). Its reachable dependency subgraph becomes the pipeline; submodule recipes appear under their fully-qualified `mod::recipe` names.

```
just --dump → root → reachable subgraph → process-compose YAML → run
```

## Modes

Gated on the `CI` environment variable:

| Mode | Trigger | Tree | Status posts | Runtime files |
|---|---|---|---|---|
| Local | `CI` unset | live working tree | none | `.ci/pc.log`, `.ci/pc.sock` |
| Strict | `CI=true` | `git worktree` pinned to HEAD | `ci/<recipe>` per transition | `.ci/pc.log`, `.ci/pc.sock`, `.ci/worktree/`, `.ci/<sha>/<recipe>.log` |

Strict mode refuses to run if the working tree is dirty — the SHA on the green check must exactly match the bytes tested. A central observer subscribes to process-compose's `/process/states/ws` stream over a Unix domain socket; in strict mode it posts a status (`pending`, then `success`/`failure`, or `error` for skipped recipes) for every state transition, and in both modes it folds each terminal state into a per-recipe outcome map. At end-of-run that map is printed as a per-recipe summary and reduced to the process's exit code (zero only if every recipe finished `Success`). Each recipe's stdout/stderr is split into its own `.ci/<sha>/<recipe>.log`, and the GitHub status `description` embeds that path — so a red check links straight to the failing log. The SHA-keyed directory keeps prior runs' logs alongside the latest. All runtime artifacts live under `$PWD/.ci/` (gitignored); process-compose binds the same UDS in both modes, so two concurrent ci runs in the same checkout collide on the socket and the second fails fast — the intended mutex.

### Cross-lane failure tolerance

Every emitted process is `restart: no` and `exit_on_skipped: false`, so one failing recipe leaves sibling lanes free to keep running and skipped dependents don't tear the project down. Process-compose's own exit code is therefore not authoritative — a failed recipe leaves pc exiting 0 — and the verdict step that consults the outcome map is what surfaces the failure.

## Subcommands

- `ci run [-- <args>]` (default): drive the pipeline; anything after `--` is forwarded verbatim to `process-compose up`.
- `ci dump-yaml`: emit the assembled YAML to stdout for inspection.

## Roadmap

- Expose process-compose's state and control surface as an [MCP](https://modelcontextprotocol.io/) server so agent CLIs can introspect mid-run.
- Remote builds over SSH: swap the `git worktree` snapshot for a `git archive` tarball uploaded to the remote host.
