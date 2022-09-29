#!/bin/sh
echo "Bootstrapping for public testnet..."
cabal run -f short-epoch bootstrap -- bootstrap.yaml
