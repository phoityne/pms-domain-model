#!/bin/sh

cabal update
cabal clean
cabal configure
cabal build
cabal test
cabal sdist
