# List all recipes.
default:
    @just --list

# Run all checks (build + flake check) concurrently, then smoke-test the binary.
[metadata("ci")]
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
# command runs `just --no-deps <recipe>` directly. Second pass exercises
# the submodule fixture — verifies the runner sees a [metadata("ci")]
# recipe living inside a submodule, emits fully-qualified process keys
# (`sub::entry`, `sub::a`, `sub::shared`), and qualifies the invocation
# argument that goes to `just --no-deps`.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- dump-yaml | tee /tmp/ci-out
    grep -qE '^  ci:' /tmp/ci-out
    grep -qE -- '--no-deps' /tmp/ci-out
    (cd test/fixtures/with-module && {{ justfile_directory() }}/result/bin/ci dump-yaml) | tee /tmp/ci-out-mod
    grep -qE '^  sub::entry:' /tmp/ci-out-mod
    grep -qE '^  sub::a:' /tmp/ci-out-mod
    grep -qE '^  sub::shared:' /tmp/ci-out-mod
    grep -qE -- '--no-deps sub::entry' /tmp/ci-out-mod

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- dump-yaml | tee "${TMPDIR%/}/ci-out"
    grep -qE '^  ci:' "${TMPDIR%/}/ci-out"
    grep -qE -- '--no-deps' "${TMPDIR%/}/ci-out"
    (cd test/fixtures/with-module && {{ justfile_directory() }}/result/bin/ci dump-yaml) | tee "${TMPDIR%/}/ci-out-mod"
    grep -qE '^  sub::entry:' "${TMPDIR%/}/ci-out-mod"
    grep -qE '^  sub::a:' "${TMPDIR%/}/ci-out-mod"
    grep -qE '^  sub::shared:' "${TMPDIR%/}/ci-out-mod"
    grep -qE -- '--no-deps sub::entry' "${TMPDIR%/}/ci-out-mod"
