#!/bin/bash
set -eu

alex tiger.x
ghc tiger.hs

tests=~/Documents/tiger/testcases
for f in $( ls $tests | grep .tig$ ); do
    echo "--${f}--"
    cat ${tests}/${f} | ./tiger
done
