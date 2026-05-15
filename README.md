# ci

A Haskell pipeline runner: translates a `just` recipe graph into a [process-compose](https://f1bonacc1.github.io/process-compose/) DAG and drives it. Sibling recipes keep running after one fails; the final exit code is derived from a per-node outcome map that the central observer accumulates, not from process-compose's own exit. In strict mode, posts per-node GitHub commit statuses live as the pipeline runs.

The pipeline root is the recipe annotated `[metadata("ci")]` — exactly one across the justfile and its submodules (zero or multiple is a startup error). Its reachable dependency subgraph becomes the pipeline; submodule recipes appear under their fully-qualified `mod::recipe` names. Each (recipe, platform) pair becomes a separate process-compose node — see *Platform fanout* below.

```
just --dump → root → reachable subgraph → fan out per platform → process-compose YAML → run
```

## Modes

Gated on the `CI` environment variable:

| Mode | Trigger | Tree | Status posts | Runtime files |
|---|---|---|---|---|
| Local | `CI` unset | live working tree | none | `.ci/pc.log`, `.ci/pc.sock` |
| Strict | `CI=true` | `git worktree` pinned to HEAD | `<recipe>@<platform>` per transition | `.ci/pc.log`, `.ci/pc.sock`, `.ci/worktree/`, `.ci/<sha>/<platform>/<recipe>.log` |

Strict mode refuses to run if the working tree is dirty — the SHA on the green check must exactly match the bytes tested. A central observer subscribes to process-compose's `/process/states/ws` stream over a Unix domain socket; in strict mode it posts a status (`pending`, then `success`/`failure`, or `error` for skipped nodes) for every state transition, and in both modes it folds each terminal state into a per-node outcome map. At end-of-run that map is printed as a per-node summary and reduced to the process's exit code (zero only if every node finished `Success`). Each node's stdout/stderr is split into its own `.ci/<sha>/<platform>/<recipe>.log`, and the GitHub status `description` embeds that path — so a red check links straight to the failing log. The SHA-keyed directory keeps prior runs' logs alongside the latest. All runtime artifacts live under `$PWD/.ci/` (gitignored); process-compose binds the same UDS in both modes, so two concurrent ci runs in the same checkout collide on the socket and the second fails fast — the intended mutex.

### Platform fanout

The pipeline's target platforms come from the root recipe's `just` OS attributes:

```just
[linux] [macos] [metadata("ci")]
ci: build run-check
```

…declares a pipeline that runs across both Linux and macOS. A root recipe with no OS attribute defaults to the local platform only (single-lane pipeline, identical to the pre-fanout shape). Each (recipe × platform) pair becomes a separate process-compose node, with `depends_on` edges replicated within each platform lane — no cross-lane edges, so a failure on one platform doesn't block the other.

Process-compose node names are `<recipe>@<platform>` (e.g. `ci::build@linux`), and the same string is the GitHub commit-status context.

### Remote builds over SSH

A node whose platform doesn't match the local host runs via SSH: the runner pipes a `git bundle` through `ssh <host>`, the remote shell clones it into a tempdir, checks out the pipeline's `HEAD` SHA, and runs `just --no-deps <recipe>` there. Per-node stdout/stderr streams back over SSH and lands in `.ci/<sha>/<platform>/<recipe>.log` exactly as a local node would.

Hosts are configured in `~/.config/ci/hosts.json`, keyed by platform:

```json
{
  "linux": "builder.example.com",
  "macos": "mac-runner.example.com"
}
```

In local mode (`CI` unset) a missing entry for a non-local platform is prompted for and persisted. In strict mode (`CI=true`) the runner refuses to start if any non-local platform is unconfigured — there's no TTY mid-run, so the prompt would deadlock the pipeline. Edit the file (or run once locally) before triggering strict-mode CI on a multi-platform root.

**Local platform override.** An entry for the *local* platform takes precedence over inline execution: configure `"linux": "pu connect srid1"` from a linux host and the linux lane routes through `pu` instead of running in the worktree. This is the path for exercising remote runners (or testing failure modes) without leaving the local box.

The remote host needs `just`, `git`, and any tools the recipes themselves use available on its PATH; no agent is installed, only the shell command stream.

#### Incus / `pu connect` runners

Hosts spelled as `pu connect <name>` in the config are routed through the `pu` incus client instead of `ssh`. The remote-command shape is identical (the prefix already names the runner+target), so a host configured as `"macos": "pu connect mac-vm"` Just Works as a drop-in:

```json
{
  "linux": "builder.example.com",
  "macos": "pu connect mac-vm"
}
```

Detection is by exact `pu connect ` prefix — anything else falls through to `ssh -T <host>`.

### Cross-lane failure tolerance

Every emitted process is `restart: no` and `exit_on_skipped: false`, so one failing node leaves sibling lanes free to keep running and skipped dependents don't tear the project down. Process-compose's own exit code is therefore not authoritative — a failed node leaves pc exiting 0 — and the verdict step that consults the outcome map is what surfaces the failure.

## Subcommands

- `ci run [-- <args>]` (default): drive the pipeline; anything after `--` is forwarded verbatim to `process-compose up`.
- `ci dump-yaml`: emit the assembled YAML to stdout for inspection. Runs in a side-effect-free mode — no host prompts, no `git rev-parse` shell-out — so it works offline, on a remote VM with no TTY, and outside a git checkout. Unresolved hosts render as `<unconfigured>` and the SSH `checkout` carries a `0000000-dump-yaml-placeholder` token; the YAML's *structure* (process keys, depends_on edges) still reflects the real fanout.

## Roadmap

- Expose process-compose's state and control surface as an [MCP](https://modelcontextprotocol.io/) server so agent CLIs can introspect mid-run.
- Per-recipe OS-attribute filtering: today a recipe is replicated to every pipeline platform regardless of its own `[linux]/[macos]` attribute (and the remote `just` refuses if the recipe isn't enabled on that host). A future pass at our layer would prune those nodes upfront so the verdict surface doesn't show them as `Failed`.
