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
nix run . -- run
```

`ci run` self-hosts: it translates the `just` recipe graph into a
`process-compose` config and drives the pipeline through `ci run-step
<recipe>` wrappers. When `CI=true` is set in the environment, each
wrapper posts a GitHub commit status (`ci/<recipe>`) at start, success,
and failure — there is no separate hosted CI workflow.

Verify via exit code and stdout (no remote CI status check needed locally).

## Documentation

- `README.md` — keep the Roadmap section in sync as items land.
