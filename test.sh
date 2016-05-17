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

F="modules/tip-benchmarks/benchmarks/tip2015/int_right_distrib.smt2"
SYMS=$(./symbols_of_theorems.rkt < "$F")
for SYM in Pos Neg Z S toInteger sign plus2 opposite timesSign mult minus plus absVal times
do
    echo "$SYMS" | grep -Fx "$SYM" > /dev/null
    report "$?" "Found '$SYM' in symbols of '$F'"
done

THEOREMS=$(echo "$SYMS" | ./theorems_from_symbols.rkt)
echo "$THEOREMS" | grep -Fx "$F" > /dev/null
report "$?" "Theorem '$F' allowed by its own symbols"

[[ "$ERR" -eq 0 ]]
report "$?" "Error code OK"

exit "$ERR"
