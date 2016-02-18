#!/usr/bin/env bash
exec "$(dirname "${BASH_SOURCE[0]}")/../common/devops-helpers/docker/push_helper.sh" \
    --repo fpco/stackage-server "$@"
