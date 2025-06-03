#!/bin/bash

# cabal-fmt
cabal-fmt -i cosense-markdown-parser.cabal

# build
cabal build

# test
cabal test
