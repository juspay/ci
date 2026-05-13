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

# Smoke-test the dump-yaml subcommand: the `ci` process is declared and its
# command goes through the run-step lifecycle wrapper (YAML may fold long
# `command` strings across lines, so we just confirm "run-step" appears).
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- dump-yaml | tee /tmp/ci-out
    grep -qE '^  ci:' /tmp/ci-out
    grep -qE 'run-step' /tmp/ci-out

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- dump-yaml | tee "${TMPDIR%/}/ci-out"
    grep -qE '^  ci:' "${TMPDIR%/}/ci-out"
    grep -qE 'run-step' "${TMPDIR%/}/ci-out"
