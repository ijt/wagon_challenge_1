#!/bin/bash

N=$1
if [ -z "$N" ]; then
  echo "Usage $0 N"
  echo " where N is the input to generator."
  exit 1
fi

.cabal-sandbox/bin/getstats

ghc -prof -fprof-auto -rtsopts getstats.hs 
time (./generator $N | ./getstats +RTS -p > /dev/null)

