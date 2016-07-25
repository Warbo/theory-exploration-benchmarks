#!/usr/bin/env bash

ERR=0

function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
        return 0
    else
        echo "not ok - $2"
        ERR=1
        return 1
    fi
}

DIR="modules/tip-benchmarks/benchmarks"
FRESH=$(find "$DIR" -name "*.smt2" | shuf | head -n10)

REGRESSIONS="$DIR/tip2015/list_elem_map.smt2
$DIR/tip2015/propositional_AndCommutative.smt2"

FILES=$(echo -e "$REGRESSIONS\n$FRESH")
COUNT=$(echo "$FILES" | wc -l)
INDEX=1

ALL_QUAL=1
while read -r FILE
do
    echo "$INDEX/$COUNT" 1>&2
    INDEX=$(( INDEX + 1 ))

    NAME=$(basename "$FILE")
    NAME2=$(echo "$NAME" | sed -e "s/'/_tick_/g") # Fix names with '

    SYMS=$(NAME="$NAME" ./qualify.rkt < "$FILE" | ./symbols_of_theorems.rkt)
    QUALIFIED=1
    while read -r SYM
    do
        QUALIFIED=0
        echo -e "Symbol '$SYM' unqualified in '$FILE'" 1>&2
    done < <(echo "$SYMS" | grep -vF "$NAME2" | grep '^.')

    [[ "$QUALIFIED" -eq 1 ]] || {
        ALL_QUAL=0
        echo -e "Symbols of '$FILE' not qualified\nSYMS:\n$SYMS\n\n" 1>&2
    }
done < <(echo "$FILES")

[[ "$ALL_QUAL" -eq 1 ]]
report "$?" "All symbols are qualified"

exit "$ERR"
