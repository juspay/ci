# ci

A Haskell playground for experiments around CI tooling.

## Usage

```sh
ci             # print the recipe graph reachable from `ci` as JSON
ci --graph     # same, explicit
ci run         # run the `ci` recipe via our scheduler
ci run <name>  # run a specific recipe
```

The runner topologically schedules a justfile's recipes, fans dep groups out
concurrently when the parent recipe has `[parallel]`, streams each recipe's
merged stdout+stderr to a per-recipe log file under `.ci-logs/`, and echoes
every line to stderr prefixed with the recipe name so concurrent output stays
attributable.

## Roadmap

- Prototype something—whilst carefully curaging srid/agency rules
- Make production
