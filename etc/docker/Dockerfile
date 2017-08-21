#fpco/stack-run:lts-5
FROM fpco/stack-run@sha256:4b170ac899755a89c0295b7726c5530015211055125f6e3f6c5b902cb3e9b74b

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    wget -qO- https://get.haskellstack.org/ | sh -x && \
    unset DEBIAN_FRONTEND

COPY _artifacts/config/ /app/config/
COPY _artifacts/static/ /app/static/
COPY _artifacts/stackage-server /usr/local/bin/stackage-server
COPY _artifacts/stackage-server-cron /usr/local/bin/stackage-server-cron
