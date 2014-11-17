#!/bin/bash -ex

cabal clean
cabal configure
cabal build
strip dist/build/cabal-loader-stackage/cabal-loader-stackage
scp dist/build/cabal-loader-stackage/cabal-loader-stackage jenkins.stackage.org:/private
