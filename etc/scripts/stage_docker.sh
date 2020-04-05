#!/usr/bin/env bash
set -ex
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
mkdir -p etc/docker/_artifacts
export LANG=C.UTF-8
stack install --local-bin-path=etc/docker/_artifacts "$@"
rsync -av --delete config static etc/docker/_artifacts/
