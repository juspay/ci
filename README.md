# ci

A Haskell playground for experiments around CI tooling.

## Roadmap

- Prototype something—whilst carefully curaging srid/agency rules
- Emit a [process-compose](https://f1bonacc1.github.io/process-compose/) config from the `just` recipe graph and drive CI with it (`cabal run` or `nix run .` executes the pipeline; `dump-yaml` prints the config; `run -- <args>` forwards extra flags to `process-compose up`)
- Post GitHub commit statuses (`ci/<recipe>`) when each recipe starts, succeeds, or fails — gated on `CI=true`, posted via `gh api`. Phase 1: executed recipes only; recipes skipped because a dep failed are deferred to Phase 2.
- Make production
