# ci

Reusable CI infrastructure for project CI skills. Provides forge-agnostic commit status posting, multisystem build (git bundle + SSH), log capture, preflight, and verification.

The orchestration logic lives in each project's own `.claude/skills/ci/SKILL.md` — this package only ships the primitives those skills delegate to.

## Install

In your project's `apm.yml`:

```yaml
dependencies:
  apm:
    - juspay/ci#main
```

`apm install` deploys the whole skill folder to `.claude/skills/ci-runtime/`:

- `SKILL.md` — protocol + primitives doc (read by your project's CI skill). Named `ci-runtime` so it doesn't collide with your project's own `ci` skill.
- `scripts/ci-{status,ssh,log,preflight,verify}` — the shims.

Requires `gh` (authenticated), `git`, `jq`, `ssh` on PATH.

## Shims

| Shim | Args | Effect |
| --- | --- | --- |
| `ci-status` | `<step> <pending\|success\|failure\|error> [description]` | POSTs a commit status with `context=ci/<step>` |
| `ci-ssh` | `<system> <cmd…>` | Bundles the repo at `$CI_SHA`, ships via SSH to host (from `~/.config/ci/hosts.json`; prompted on first use), runs `cmd` |
| `ci-log` | `<step>` | Emits `.logs/<short-sha>/<step>.log` (creates parent dir) |
| `ci-preflight` | — | Asserts clean worktree + HEAD pushed; emits the sha |
| `ci-verify` | `<step…>` | Cross-checks posted statuses for `$CI_SHA` against the listed steps; table out; non-zero on missing/non-success |

All shims read `$CI_SHA` if set; otherwise resolve `git rev-parse HEAD` themselves. Repo is derived from `git remote get-url origin`.

## Forge

GitHub today via `gh`. Bitbucket support will land as an internal branch in `ci-status` and `ci-verify`; callers don't change.

## Authoring a project CI skill

See `.apm/skills/ci-runtime/SKILL.md` for the full protocol. Short version: write `.apm/skills/ci/SKILL.md` in your project (deploys to `.claude/skills/ci/SKILL.md`), list your steps in markdown (name, command, optional `depends_on`, optional `system`), and follow the orchestration protocol from `ci-runtime`. The SKILL.md *is* the manifest — no separate config file.
