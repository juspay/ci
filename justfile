# List all recipes.
default:
    @just --list

# Build and smoke-test the package.
ci: build run-check

# Build the package with Nix.
build:
    nix build --print-build-logs

# Watch sources and auto-recompile + re-run main on change.
ghcid:
    ghcid -T :main

# Run the binary and verify its JSON output contains the `ci` recipe.
run-check: build
    nix run . | tee /tmp/ci-out
    grep -q '"ci"' /tmp/ci-out
