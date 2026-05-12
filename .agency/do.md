# do

Project hooks for the `do` skill.

## Check command

```sh
nix develop -c cabal build
```

## Test command

```sh
just run-check runner-check
```

## CI command

```sh
just ci
```

Verify via exit code and stdout (no remote CI status check needed locally).

## Documentation

- `README.md` — keep the Roadmap section in sync as items land.
