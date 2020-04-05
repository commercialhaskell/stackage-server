FROM docker.pkg.github.com/fpco/stackage-server/base-build:dcc4ec7213daed35b27629b894c06a9769b9acff as build-app

RUN mkdir -p /artifacts/bin
COPY . /src
RUN stack install --stack-yaml /src/stack.yaml --local-bin-path /artifacts/bin

FROM docker.pkg.github.com/fpco/stackage-server/base-run:dcc4ec7213daed35b27629b894c06a9769b9acff

COPY --from=build-app /src/config/ /app/config/
COPY --from=build-app /src/static/ /app/static/
COPY --from=build-app /artifacts/bin/stackage-server /usr/local/bin/stackage-server
COPY --from=build-app /artifacts/bin/stackage-server-cron /usr/local/bin/stackage-server-cron
