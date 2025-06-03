#!/bin/bash

# cabal-fmt
cabal-fmt -i cosense-markdown-parser

# build
cabal build

# test
cabal test
