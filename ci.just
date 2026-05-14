# Run all checks (build + flake check) concurrently, then smoke-test the binary.
[metadata("ci")]
default: checks run-check

# Fan out `build` and `flake-check` to run in parallel.
[parallel]
checks: build flake-check

# Build the package with Nix.
build:
    nix build --print-build-logs

# Run the flake's checks (formatting, hlint, etc. via haskell-flake).
flake-check:
    nix flake check --print-build-logs

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

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- dump-yaml | tee "${TMPDIR%/}/ci-out"
    grep -qE '^  ci::default:' "${TMPDIR%/}/ci-out"
    grep -qE -- '--no-deps' "${TMPDIR%/}/ci-out"
