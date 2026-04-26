---
name: ci-runtime
description: Reusable CI infrastructure — signoff (commit status posting), multisystem build (git bundle + SSH), log capture, orchestration protocol. Project CI skills delegate to this; do not invoke directly.
---

# CI Runtime

Reusable infrastructure for project CI skills. The project's own `ci` skill lists its steps inline (markdown table); this skill defines how to run them, post statuses, route remote work, and capture logs.

Two ways to use it:

- **Spawn the `/ci-runtime` agent** (recommended). The project skill passes the step table; the agent runs the protocol and returns a summary. Early-exits on first failure so the calling session can read the log and start a fix while the rest of CI keeps running.
- **Follow the protocol inline.** The project skill executes the protocol itself in the main thread — every step's notifications are visible in chat. Useful when you want maximum visibility and don't mind the context noise.

## Primitives

After `apm install`, the shim scripts live in this skill's `scripts/` folder. The orchestrator adds them to PATH once at start; nothing else hardcodes a path:

```bash
ci_bin="$(realpath .claude/skills/ci-runtime/scripts)"
export PATH="$ci_bin:$PATH"
export CI_BIN="$ci_bin"   # pass to subagents
```

Then call shims by bare name:

- **`ci-status <step> <pending|success|failure|error> [description]`** — post a commit status via the configured forge (GitHub today; Bitbucket via the same shim later).
- **`ci-ssh <system> <cmd…>`** — git-bundle the repo at `$CI_SHA`, ship to the target host, run, stream output, clean up. Host map: `~/.config/ci/hosts.json` (prompted on first use).
- **`ci-log <step>`** — emit `.logs/<short-sha>/<step>.log`.
- **`ci-preflight`** — assert clean worktree + HEAD pushed; emit the resolved sha on stdout.
- **`ci-verify <step…>`** — cross-check posted statuses for `$CI_SHA` against the listed steps; print a table; non-zero on missing/non-success.

All shims read `$CI_SHA` if set; otherwise resolve `git rev-parse HEAD`.

## Orchestration protocol

1. **Preflight.** `export CI_SHA=$(ci-preflight)`. Fail-fast on dirty/unpushed; pin sha for the run.
2. **Plan.** Parse the project's step table — names, commands, optional `depends_on`, optional `system`. Build the DAG.
3. **Execute.** For each ready wave:
   - Spawn one subagent per ready step on the **cheapest model** the harness offers (Claude Code: `haiku`; Codex: `gpt-5-mini`; opencode: per-config). Step subagents are mechanical executors — no reasoning needed.
   - Each step subagent: export PATH/CI_SHA, `ci-status <step> pending`, run command (or `ci-ssh <system> <cmd>` if `system` is set), tee to `$(ci-log <step>)`, post final `success "<elapsed>s"` or `failure "failed after <elapsed>s · <log>"`.
   - Mark dependents of failed steps `failure "blocked by <upstream>"` and skip their commands.
4. **Verify.** `ci-verify <every step>`. Missing context = silent failure — report loudly.
5. **Summarize.** One-line-per-step table; non-zero exit if any failed or missing.

## Step name discipline

Branch protection pins literal context strings. Use the canonical step name from the project's SKILL.md verbatim — no paraphrasing, abbreviating, or suffixing.

## Authoring a project CI skill

Write `.apm/skills/ci/SKILL.md` in your project. List your steps in markdown — name, command, optional `depends_on`, optional `system`. The skill body says "spawn the `/ci-runtime` agent with this step table". The SKILL.md *is* the manifest; no separate config file.
