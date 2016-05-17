#!/usr/bin/env bash

ERR=0

F="modules/tip-benchmarks/benchmarks/tip2015/int_right_distrib.smt2"
SYMS=$(./symbols_of_theorems.rkt < "$F")
for SYM in Pos Neg Z S toInteger sign plus2 opposite timesSign mult minus plus absVal times
do
    MSG="Found '$SYM' in symbols of '$F'"
    if echo "$SYMS" | grep -Fx "$SYM" > /dev/null
    then
        echo "ok - $MSG"
    else
        echo "not ok - $MSG"
        ERR=1
    fi
done

MSG="Error code OK"
if [[ "$ERR" -eq 0 ]]
then
    echo "ok - $MSG"
else
    echo "not ok - $MSG"
fi

exit "$ERR"
