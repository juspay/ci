# Run all checks (build + flake check) concurrently, then smoke-test the binary
# on each target platform. The [linux] [macos] attributes fan the pipeline out
# across both lanes: every reachable recipe gets a (recipe, platform) node, the
# local lane runs against the worktree, and the macos lane SSHs to whatever
# `~/.config/ci/hosts.json` maps `macos` to.
[linux]
[macos]
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

# Smoke-test the dump-yaml subcommand. The pipeline root is `ci::default`,
# tagged [linux] [macos] [metadata("ci")] — so the fanout produces both
# lanes and every process key is `ci::<name>@<platform>`. Each platform's
# smoke verifies its own lane is present plus the structural invariants
# (--no-deps, restart: no, exit_on_skipped: false) for cross-lane failure
# tolerance. `dump-yaml` runs in `DumpRun` mode so it never prompts for
# hosts and never shells out to git — safe to invoke on the macos remote
# even when `~/.config/ci/hosts.json` there has no linux entry.
[linux]
run-check: build
    echo "Running on Linux $(uname -srm)"
    nix run . -- dump-yaml | tee /tmp/ci-out
    grep -qE '^  ci::default@linux:' /tmp/ci-out
    grep -qE '^  ci::default@macos:' /tmp/ci-out
    grep -qE -- '--no-deps' /tmp/ci-out
    grep -qE "^      restart: 'no'$" /tmp/ci-out
    grep -qE '^      exit_on_skipped: false$' /tmp/ci-out

[macos]
run-check: build
    echo "Running on macOS $(sw_vers -productVersion)"
    nix run . -- dump-yaml | tee "${TMPDIR%/}/ci-out"
    grep -qE '^  ci::default@linux:' "${TMPDIR%/}/ci-out"
    grep -qE '^  ci::default@macos:' "${TMPDIR%/}/ci-out"
    grep -qE -- '--no-deps' "${TMPDIR%/}/ci-out"
    grep -qE "^      restart: 'no'$" "${TMPDIR%/}/ci-out"
    grep -qE '^      exit_on_skipped: false$' "${TMPDIR%/}/ci-out"
