# List all recipes.
default:
    @just --list

# Run all checks (build + flake check) concurrently, then smoke-test the binary.
[metadata("entrypoint")]
ci: checks run-check

# Fan out `build` and `flake-check` to run in parallel.
[parallel]
checks: build flake-check

# Build the package with Nix.
build:
    nix build --print-build-logs

# Run the flake's checks (formatting, hlint, etc. via haskell-flake).
flake-check:
    nix flake check --print-build-logs

# Watch sources and auto-recompile + re-run main on change.
ghcid:
    ghcid -T :main

# Run the binary and verify its JSON output contains the `ci` recipe.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . | tee /tmp/ci-out
    grep -q '"ci"' /tmp/ci-out

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . | tee "${TMPDIR}ci-out"
    grep -q '"ci"' "${TMPDIR}ci-out"
