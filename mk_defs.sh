#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

# Combine all definitions in files given on stdin

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

while read -r FILE
do
    NAME=$(basename "$FILE")
     DIR=$(basename "$(dirname "$FILE")")
    NAME="$DIR/$NAME" racket qualify.rkt < "$FILE" | fixup | trim
done
