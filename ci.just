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

# Smoke-test the dump-yaml subcommand. The pipeline root is `ci::default`
# (this module's default recipe, tagged [metadata("ci")]) — no OS attrs,
# so the platform set defaults to the local platform only and every
# process key is `ci::<name>@<platform>`. The `@<platform>` suffix proves
# the system-axis fanout is wired through end-to-end; the `ci::<name>`
# prefix proves submodule discovery still works.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- dump-yaml | tee /tmp/ci-out
    grep -qE '^  ci::default@linux:' /tmp/ci-out
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
    grep -qE '^  ci::default@macos:' "${TMPDIR%/}/ci-out"
    grep -qE -- '--no-deps' "${TMPDIR%/}/ci-out"
    grep -qE "^      restart: 'no'$" "${TMPDIR%/}/ci-out"
    grep -qE '^      exit_on_skipped: false$' "${TMPDIR%/}/ci-out"
