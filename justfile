default:
    @just --list

ci: build run-check

build:
    nix build --print-build-logs

ghcid:
    ghcid -T :main

run-check: build
    nix run . | tee /tmp/ci-out
    grep -q '"ci"' /tmp/ci-out
