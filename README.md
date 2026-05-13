# ci

A Haskell playground for experiments around CI tooling.

## Roadmap

- Prototype something—whilst carefully curaging srid/agency rules
- Emit a [process-compose](https://f1bonacc1.github.io/process-compose/) config from the `just` recipe graph and drive CI with it (`cabal run` or `nix run .` executes the pipeline; `dump-yaml` prints the config)
- Make production
