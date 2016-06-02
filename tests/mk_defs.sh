#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

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

function path {
    # Prefix our argument with the benchmarks directory, to save us typing it
    # over and over
    echo "modules/tip-benchmarks/benchmarks/$1"
}

function haveDef {
    DEF=$(echo "$DEFS" | ./get_fun_def.sh "$1-sentinel")
    COUNT=$(echo "$DEF" | grep '^.' | wc -l)

    [[ "$COUNT" -eq 1 ]]
    report "$?" "Can get $2 function definition" || {
        echo -e "DEFS:\n$DEFS\n\nDEF:\n$DEF\n\n" 1>&2
    }
}

DEFS=$(path "tip2015/sort_StoogeSort2IsSort.smt2" | ./mk_defs.sh)

haveDef "tip2015/sort_StoogeSort2IsSort.smt2sort2"        "plain"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2insert2"      "recursive"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2zsplitAt"     "parameterised"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2ztake"        "parameterised recursive"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2" "mutually recursive"

FILES="modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig3.smt2
modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig1.smt2
modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig2.smt2
modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig5.smt2
modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig4.smt2
modules/tip-benchmarks/benchmarks/grammars/packrat_unambigPackrat.smt2
modules/tip-benchmarks/benchmarks/isaplanner/prop_54.smt2
modules/tip-benchmarks/benchmarks/isaplanner/prop_37.smt2
modules/tip-benchmarks/benchmarks/isaplanner/prop_45.smt2
modules/tip-benchmarks/benchmarks/isaplanner/prop_79.smt2"

DEFS=$(echo "$FILES" | ./mk_defs.sh)
SYMS=$(echo "$DEFS"  | ./symbols_of_theorems.rkt)

for SYM in grammars/simp_expr_unambig3.smt2append-sentinel     \
           grammars/simp_expr_unambig3.smt2lin-sentinel        \
           grammars/simp_expr_unambig1.smt2append-sentinel     \
           grammars/simp_expr_unambig1.smt2lin-sentinel        \
           grammars/simp_expr_unambig2.smt2append-sentinel     \
           grammars/simp_expr_unambig2.smt2lin-sentinel        \
           grammars/simp_expr_unambig5.smt2linTerm-sentinel    \
           grammars/simp_expr_unambig5.smt2append-sentinel     \
           grammars/simp_expr_unambig5.smt2lin-sentinel        \
           grammars/simp_expr_unambig4.smt2append-sentinel     \
           grammars/simp_expr_unambig4.smt2linTerm-sentinel    \
           grammars/simp_expr_unambig4.smt2lin-sentinel        \
           grammars/packrat_unambigPackrat.smt2append-sentinel \
           grammars/packrat_unambigPackrat.smt2linA-sentinel   \
           grammars/packrat_unambigPackrat.smt2linB-sentinel   \
           grammars/packrat_unambigPackrat.smt2linS-sentinel   \
           isaplanner/prop_54.smt2plus-sentinel                \
           isaplanner/prop_54.smt2minus-sentinel               \
           isaplanner/prop_37.smt2equal-sentinel               \
           isaplanner/prop_37.smt2elem-sentinel                \
           isaplanner/prop_37.smt2delete-sentinel              \
           isaplanner/prop_45.smt2zip-sentinel                 \
           isaplanner/prop_79.smt2minus-sentinel
do
    echo "$SYMS" | grep -Fx "$SYM" > /dev/null
    report "$?" "Found '$SYM' in symbols"
done

DUPES=0
NORMALISED=""
while read -r SYM
do
    DEF=$(echo "$DEFS" | ./get_def.sh "$SYM")
    COUNT=$(echo "$DEF" | grep '^.' | wc -l)

    [[ "$COUNT" -eq 1 ]]
    report "$?" "Got definition for '$SYM'" || {
        echo -e "SYM: $SYM\nDEF:\n$DEF\n\n" 1>&2
    }

    if echo "$DEF" | grep "(declare-datatypes" > /dev/null
    then
        # SYM is a type or constructor
        true
    else
        # SYM is a function
        MSG="Normalised '$SYM' isn't a duplicate"

        CANON=$(echo "$DEF"  | ./canonical_functions.rkt)
        if echo "$NORMALISED" | grep -Fx "$CANON" | grep '^.' > /dev/null
        then
            report 1 "$MSG"
            DUPES=1
            echo -e "SYM: $SYM\nDEF: $DEF\nCANON: $CANON\nNORMALISED:\n$NORMALISED\n\n" 1>&2
        else
            report 0 "$MSG"
        fi
        NORMALISED=$(echo -e "$NORMALISED\n$CANON")
    fi
done < <(echo "$SYMS")

[[ "$DUPES" -eq 0 ]]
report "$?" "No duplicate functions"

exit "$ERR"
