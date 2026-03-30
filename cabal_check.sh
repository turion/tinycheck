#! /usr/bin/env bash

set -e

cabal check
cabal-gild --io=tinycheck.cabal
git diff --exit-code
