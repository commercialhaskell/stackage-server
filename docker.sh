#!/usr/bin/env bash

set -exu

TAG=$(git rev-parse --short HEAD)

(
rm -rf docker/app
mkdir -p docker/app
yesod keter
cd docker/app
tar xf ../../stackage-server.keter
cd ..
docker build -t snoyberg/stackage-server:$TAG .
docker push snoyberg/stackage-server:$TAG
)
