#!/usr/bin/env bash

set -exu

(
rm -rf docker/app
mkdir -p docker/app
yesod keter
cd docker/app
tar xf ../../stackage-server.keter
cd ..
docker build -t snoyberg/stackage-server .
docker push snoyberg/stackage-server
)
