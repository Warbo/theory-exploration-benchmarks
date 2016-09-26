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

# Check that definitions which normalise to the same thing are deduped

GIVEN="
(define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
(define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
"

DEFS=$(echo "$GIVEN" | ./norm_defs.sh "example")
SYMS=$(echo "$DEFS"  | ./symbols_of_theorems.rkt)

MINS=0
echo "$SYMS" | grep "min1" > /dev/null && MINS=$(( MINS + 1 ))
echo "$SYMS" | grep "min2" > /dev/null && MINS=$(( MINS + 1 ))
[[ "$MINS" -eq 1 ]]
report "$?" "Simple redundant functions deduped" ||
    echo -e "DEFS:\n$DEFS\n\nSYMS:\n$SYMS\n\nMINS: $MINS" 1>&2
unset MINS

GIVEN="
(define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
(define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
(define-fun fun3 ((x Int)) Int (min2 x x))
"

DEFS=$(echo "$GIVEN" | ./norm_defs.sh "example")
SYMS=$(echo "$DEFS"  | grep "fun3" | ./symbols_of_theorems.rkt)

echo "$SYMS" | grep "min1" > /dev/null
report "$?" "References to discarded duplicates are replaced"

exit "$ERR"
