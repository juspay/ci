# Run all checks (build + flake check) concurrently, then smoke-test the binary.
[metadata("ci")]
default: checks run-check

# Fan out `build`, `flake-check`, and `fmt-check` to run in parallel.
[parallel]
checks: build flake-check fmt-check

# Build the package with Nix.
build:
    nix build --print-build-logs

# Run the flake's checks (formatting, hlint, etc. via haskell-flake).
flake-check:
    nix flake check --print-build-logs

# Fail the build if any Haskell source isn't fourmolu-formatted. The
# dev shell exposes fourmolu via haskell-flake; --mode check exits
# non-zero on any drift and prints a unified diff of what would change.
# Style settings live in `fourmolu.yaml` at the repo root.
fmt-check:
    nix develop -c fourmolu --mode check $(find src test app -name '*.hs')

# Smoke-test the dump-yaml subcommand: the pipeline root is `ci::default`
# (this module's default recipe, tagged [metadata("ci")]) so every process
# in the YAML is a fully-qualified `ci::<name>` — proving submodule
# discovery is wired through.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- dump-yaml | tee /tmp/ci-out
    grep -qE '^  ci::default:' /tmp/ci-out
    grep -qE -- '--no-deps' /tmp/ci-out
    # Cross-lane failure tolerance: every process emits `restart: no` and
    # `exit_on_skipped: false` so one failing recipe leaves siblings
    # running. The verdict step derives the final exit code from the
    # accumulated outcomes, not pc's own exit.
    grep -qE "^      restart: 'no'$" /tmp/ci-out
    grep -qE '^      exit_on_skipped: false$' /tmp/ci-out

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- dump-yaml | tee "${TMPDIR%/}/ci-out"
    grep -qE '^  ci::default:' "${TMPDIR%/}/ci-out"
    grep -qE -- '--no-deps' "${TMPDIR%/}/ci-out"
    grep -qE "^      restart: 'no'$" "${TMPDIR%/}/ci-out"
    grep -qE '^      exit_on_skipped: false$' "${TMPDIR%/}/ci-out"
