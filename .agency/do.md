# do

Project hooks for the `do` skill.

## Check command

```sh
nix develop -c cabal build
```

## Test command

```sh
just run-check
```

## CI command

```sh
CI=true nix run . -- run
```

`ci run` self-hosts: it translates the `just` recipe graph into a
`process-compose` config and drives the pipeline through `ci run-step
<recipe>` wrappers. `CI=true` flips each wrapper into status-posting
mode, so a GitHub commit status (`ci/<recipe>`) is posted at start,
success, and failure for every recipe — there is no separate hosted
CI workflow.

Verify via exit code and stdout (no remote CI status check needed locally).

## Documentation

- `README.md` — the user-facing description of what `ci` does. Update when:
  - CLI surface changes (subcommands, flags, env-var gates).
  - Mode behavior changes (what local vs strict do).
  - Runtime-artifact layout changes (`.ci/` contents).
  - Recipe-graph conventions change (e.g. the entrypoint metadata).
  - A Roadmap item lands (delete the bullet) or a new direction is committed to (add a bullet).
