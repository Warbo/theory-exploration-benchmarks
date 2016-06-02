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

function checkNormal {
    CANON=$(echo "$2" | ./canonical_functions.rkt)

    [[ "x$CANON" = "x$3" ]]
    report "$?" "Normalising $1 as expected"
}

# Check that each form normalises as we expect

checkNormal "function" \
            "(define-fun sort2 ((x Int) (y Int)) (list Int) (ite (<= x y) (cons x (cons y (as nil (list Int)))) (cons y (cons x (as nil (list Int))))))" \
            "(define-fun defining-function-1 ((normalise-var-2 Int) (normalise-var-1 Int)) (list Int) ((((ite (<= normalise-var-2 normalise-var-1) (cons normalise-var-2 (cons normalise-var-1 (as nil (list Int)))) (cons normalise-var-1 (cons normalise-var-2 (as nil (list Int)))))))))"

checkNormal "parameterised function" \
            "(define-fun (par (a) (zsplitAt ((x Int) (y (list a))) (Pair (list a) (list a)) (Pair2 (ztake x y) (zdrop x y)))))" \
            "(define-fun (par (normalise-var-3) (((defining-function-1 ((normalise-var-2 Int) (normalise-var-1 (list normalise-var-3))) (Pair (list normalise-var-3) (list normalise-var-3)) (((Pair2 (ztake normalise-var-2 normalise-var-1) (zdrop normalise-var-2 normalise-var-1)))))))))"

checkNormal "datatype" \
            "(declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))" \
            "(declare-datatypes (normalise-var-1) (((defining-type-1 (normalise-constructor-2) (normalise-constructor-1 (normalise-destructor-2 normalise-var-1) (normalise-destructor-1 (defining-type-1 normalise-var-1)))))))"

# Check that definitions which normalise to the same thing are deduped

GIVEN="
(define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
(define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
"

DEFS=$(echo "$GIVEN" | ./norm_defs.sh "example")
SYMS=$(echo "$DEFS" | ./symbols_of_theorems.sh)

MINS=0
echo "$SYMS" | grep "min1" > /dev/null && MINS=$(( MINS + 1 ))
echo "$SYMS" | grep "min2" > /dev/null && MINS=$(( MINS + 1 ))
[[ "$MINS" -eq 1 ]]
report "$?" "Simple redundant functions deduped"
unset MINS

GIVEN="
(define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
(define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
(define-fun fun3 ((x Int)) Int (min2 x x))
"

DEFS=$(echo "$GIVEN" | ./norm_defs.sh "example")
SYMS=$(echo "$DEFS"  | grep "fun3" | ./symbols_of_theorems.sh)

echo "$SYMS" | grep "min1" > /dev/null
report "$?" "References to discarded duplicates are replaced"

echo "$DEFS"

exit "$ERR"
