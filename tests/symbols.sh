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

function contains {
    grep -Fx "$1" > /dev/null
}

function shouldFind {
    for SYM in "$@"
    do
        echo "$SYMS" | contains "$SYM"
        report "$?" "Symbols of '$F' should contain $TYP '$SYM'"
    done
}

function shouldNotFind {
    for SYM in "$@"
    do
        ! echo "$SYMS" | contains "$SYM"
        report "$?" "Symbols of '$F' shouldn't contain $TYP '$SYM'"
    done
}

F="modules/tip-benchmarks/benchmarks/tip2015/int_right_distrib.smt2"
SYMS=$(./symbols_of_theorems.rkt < "$F")

TYP="constructor"
shouldFind Pos Neg Z S P N

TYP="function"
shouldFind toInteger sign plus2 opposite timesSign mult minus plus absVal times

TYP="type"
shouldNotFind Nat Sign Integer

TYP="variable"
shouldNotFind x y z m m2 n n2 n3 o p P_0 N_0

TYP="keyword"
shouldNotFind match case define-fun declare-datatypes define-fun assert-not \
              forall = check-sat

THEOREMS=$(echo "$SYMS" | ./theorems_from_symbols.rkt)
echo "$THEOREMS" | contains "$F"
report "$?" "Theorem '$F' allowed by its own symbols"

[[ "$ERR" -eq 0 ]]
report "$?" "Error code OK"

exit "$ERR"
