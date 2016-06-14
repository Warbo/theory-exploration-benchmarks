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

function contains {
    grep -Fx "$1" > /dev/null
}

function shouldFind {
    for SYM in "$@"
    do
        echo "$SYMS" | contains "$SYM" || {
            echo -e "Didn't find '$SYM' in:\n$SYMS\n\n" 1>&2
            return 1
        }
    done
    return 0
}

function shouldNotFind {
    for SYM in "$@"
    do
        echo "$SYMS" | contains "$SYM" && {
            echo -e "Found '$SYM' in:\n$SYMS\n\n" 1>&2
            return 1
        }
        echo "$SYMS" | contains "$SYM-sentinel" && {
            echo -e "Found '$SYM' in:\n$SYMS\n\n" 1>&2
            return 1
        }
    done
    return 0
}

F="modules/tip-benchmarks/benchmarks/tip2015/int_right_distrib.smt2"
SYMS=$(./symbols_of_theorems.rkt < "$F")
report "$?" "Can get symbols from '$F'"

shouldFind Pos Neg Z S P N
report "$?" "Found constructors"

shouldFind p P_0 N_0
report "$?" "Found destructors"

shouldFind toInteger sign plus2 opposite timesSign mult minus plus absVal times
report "$?" "Found functions"

shouldNotFind Nat Sign Integer "=>"
report "$?" "Types stripped"

shouldNotFind x y z m m2 n n2 n3 o
report "$?" "Variables stripped"

shouldNotFind match case define-fun declare-datatypes define-fun assert-not \
              forall = check-sat
report "$?" "Keywords stripped"

THEOREMS=$(echo "$SYMS" | ./theorems_from_symbols.rkt)
report "$?" "Got theorems from symbols"

echo "$THEOREMS" | contains "$F"
report "$?" "Theorem '$F' allowed by its own symbols" ||
    echo -e "SYMS:\n$SYMS\n\nTHEOREMS:\n$THEOREMS\n\nF: $F" 1>&2

F="modules/tip-benchmarks/benchmarks/tip2015/list_PairEvens.smt2"
SYMS=$(./symbols_of_theorems.rkt < "$F")
report "$?" "Can get symbols from '$F'"

shouldNotFind "=>"
report "$?" "Higher-order types stripped"

F="modules/tip-benchmarks/benchmarks/tip2015/propositional_AndCommutative.smt2"
SYMS=$(./symbols_of_theorems.rkt < "$F")
report "$?" "Can get symbols from '$F'"

shouldFind "or2"
report "$?" "Found 'or2'"

exit "$ERR"
