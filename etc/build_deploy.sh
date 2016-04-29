#!/usr/bin/env bash
# See description at https://github.com/fpco/devops-helpers#wrappers
set -xe
cd "$(dirname "${BASH_SOURCE[0]}")/.."
ENV="$1"; shift
stack --docker --docker-auto-pull test "$@"
etc/docker/build.sh --no-build "$@"
etc/docker/push.sh "$ENV"
etc/kubernetes/deploy_rc.sh "$ENV"
