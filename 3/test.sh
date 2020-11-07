#!/bin/bash
set -eu

alex -o Lexer.hs tiger.x
happy -o Main.hs -a -d tiger.y
ghc -o tiger Main.hs

tests=~/Documents/tiger/testcases
for f in $( ls $tests | grep test | grep .tig$ ); do
    echo "--${f}--"
    if grep "/* error: syntax error" ${tests}/${f}; then
        echo 'skipped'
    else
        cat ${tests}/${f} | ./tiger
    fi
done
