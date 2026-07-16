#!/usr/bin/env bash
set -eu

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMAGE="biomastats-check:local"

docker build --tag "$IMAGE" --file "$ROOT/docker/Dockerfile" "$ROOT"
docker run --rm "$IMAGE"
