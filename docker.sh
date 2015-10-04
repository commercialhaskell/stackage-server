#!/usr/bin/env bash

set -exu

TAG=$(git rev-parse --short HEAD)

(
rm -rf docker/app
mkdir -p docker/app
stack build
cp $(stack exec which stackage-server) docker/app
cp -r static config docker/app
cd docker
docker build -t snoyberg/stackage-server:$TAG .
docker push snoyberg/stackage-server:$TAG
)

echo Pushed as snoyberg/stackage-server:$TAG
