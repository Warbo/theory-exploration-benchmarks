#!/usr/bin/env bash

ERR=0
function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
    else
        echo "not ok - $2"
        ERR=1
    fi
}

for SYM in Pos Neg Z S P N
do
    FOUND=$(echo "$SYM" | ./constructor_def.rkt)
    [[ -n "$FOUND" ]]
    report "$?" "Found at least one definition of '$SYM'"

    COUNT=$(echo "$FOUND" | wc -l)
    [[ "$COUNT" -eq 1 ]]
    report "$?" "Found exactly one definition of '$SYM'"
done

for SYM in toInteger sign plus2 opposite timesSign mult minus plus absVal times
do
    FOUND=$(echo "$SYM" | ./function_def.rkt)
    [[ -n "$FOUND" ]]
    report "$?" "Found at least one definition of '$SYM'"

    COUNT=$(echo "$FOUND" | wc -l)
    [[ "$COUNT" -eq 1 ]]
    report "$?" "Found exactly one definition of '$SYM'"
done
