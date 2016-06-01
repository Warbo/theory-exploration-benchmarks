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

while read -r FILE
do
    NAME=$(basename "$FILE")
    SYMS=$(NAME="$NAME" ./qualify.rkt < "$FILE" | ./symbols_of_theorems.rkt)
    QUALIFIED=1
    while read -r SYM
    do
        QUALIFIED=0
    done < <(echo "$SYMS" | grep -vF "$NAME")

    [[ "$QUALIFIED" -eq 1 ]]
    report "$?" "All symbols in '$FILE' are qualified" ||
        echo -e "SYMS:\n$SYMS\n\n" 1>&2
done < <(find modules/tip-benchmarks/benchmarks -name "*.smt2" | shuf | head -n10)

exit "$ERR"
