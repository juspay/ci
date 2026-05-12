# List all recipes.
default:
    @just --list

# Run all checks (build + flake check) concurrently, then smoke-test the binary.
[metadata("entrypoint")]
ci: checks run-check runner-check

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

# Run the binary in graph mode and verify its JSON contains the `ci` recipe.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- graph | tee /tmp/ci-out
    grep -q '"ci"' /tmp/ci-out

# Run the binary in graph mode and verify its JSON contains the `ci` recipe.
[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- graph | tee "${TMPDIR%/}/ci-out"
    grep -q '"ci"' "${TMPDIR%/}/ci-out"

# Smoke-test the in-Haskell runner: execute `noop` via the binary and verify
# both the prefixed live tail (stderr) and the per-recipe log file landed.
runner-check: build
    rm -rf .ci-logs
    nix run . -- run noop 2>&1 | tee /tmp/runner-out
    grep -q '^noop | noop ran$' /tmp/runner-out
    grep -q '^noop ran$' .ci-logs/noop.log

# Trivial leaf recipe used by runner-check.
noop:
    echo "noop ran"
