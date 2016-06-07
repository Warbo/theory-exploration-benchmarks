#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

function trim {
    grep -v "^(assert-not " |
        grep -v "^(check-sat)"
}

function fixup {
    TO_FIX=$(cat)
    for NATIVE in Bool true false or and ite "=>"
    do
        TO_FIX=$(echo "$TO_FIX" | sed -e "s@[a-zA-Z0-9/._]*.smt2$NATIVE@$NATIVE@g")
    done
    echo "$TO_FIX"
}

# Combine all definitions in files given on stdin

racket qual_all.rkt | fixup | trim
