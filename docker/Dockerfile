FROM ubuntu:14.04

RUN DEBIAN_FRONTEND=noninteractive apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y libpq5 libgmp10 git

ADD app /app
WORKDIR /app

ENTRYPOINT ./dist/build/stackage-server/stackage-server Production
EXPOSE 3000
