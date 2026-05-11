default:
    @just --list

build:
    nix build --print-build-logs

run-check:
    nix run . | tee /tmp/ci-out
    grep "Hello, World!" /tmp/ci-out
