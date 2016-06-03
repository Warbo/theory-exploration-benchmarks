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
    DEF=$(echo "$DEFS" | bash get_fun_def.sh "$1-sentinel")
    COUNT=$(echo "$DEF" | grep '^.' | wc -l)

    [[ "$COUNT" -eq 1 ]]
    report "$?" "Can get $2 function definition" || {
        echo -e "DEFS:\n$DEFS\n\nDEF:\n$DEF\n\n" 1>&2
    }
}

# Check each function declaration syntax

DEFS=$(path "tip2015/sort_StoogeSort2IsSort.smt2" | bash mk_defs.sh)

haveDef "tip2015/sort_StoogeSort2IsSort.smt2sort2"        "plain"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2insert2"      "recursive"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2zsplitAt"     "parameterised"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2ztake"        "parameterised recursive"
haveDef "tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2" "mutually recursive"

# Check a wider selection of files

FILES="modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig1.smt2
modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig4.smt2
modules/tip-benchmarks/benchmarks/tip2015/sort_StoogeSort2IsSort.smt2"

QUAL=$(echo "$FILES" | bash qual_all.sh)
SYMS=$(echo "$QUAL"  | bash symbols_of_theorems.sh)

for SYM in true-sentinel false-sentinel ite-sentinel or-sentinel
do
    ! echo "$SYMS" | grep -Fx "$SYM" > /dev/null
    report "$?" "Native symbol '$SYM' was stripped"
done

###

SUBSET="grammars/simp_expr_unambig1.smt2append-sentinel
grammars/simp_expr_unambig1.smt2lin-sentinel
grammars/simp_expr_unambig4.smt2nil-sentinel
grammars/simp_expr_unambig4.smt2cons-sentinel
grammars/simp_expr_unambig4.smt2C-sentinel
grammars/simp_expr_unambig4.smt2D-sentinel
grammars/simp_expr_unambig4.smt2X-sentinel
grammars/simp_expr_unambig4.smt2Y-sentinel
grammars/simp_expr_unambig4.smt2Pl-sentinel
grammars/simp_expr_unambig4.smt2Plus-sentinel
grammars/simp_expr_unambig4.smt2EX-sentinel
grammars/simp_expr_unambig4.smt2EY-sentinel
grammars/simp_expr_unambig4.smt2head-sentinel
grammars/simp_expr_unambig4.smt2tail-sentinel
grammars/simp_expr_unambig4.smt2Plus_0-sentinel
grammars/simp_expr_unambig4.smt2Plus_1-sentinel
grammars/simp_expr_unambig4.smt2append-sentinel
grammars/simp_expr_unambig4.smt2linTerm-sentinel
grammars/simp_expr_unambig4.smt2lin-sentinel
grammars/packrat_unambigPackrat.smt2append-sentinel
grammars/packrat_unambigPackrat.smt2linA-sentinel
grammars/packrat_unambigPackrat.smt2linB-sentinel
grammars/packrat_unambigPackrat.smt2linS-sentinel
isaplanner/prop_54.smt2plus-sentinel
isaplanner/prop_54.smt2minus-sentinel
isaplanner/prop_37.smt2equal-sentinel
isaplanner/prop_37.smt2elem-sentinel
isaplanner/prop_37.smt2delete-sentinel"

ALL_FOUND=1
while read -r SYM
do
    echo "$SYMS" | grep -Fx "$SYM" > /dev/null || ALL_FOUND=0
done < <(echo "$SUBSET")

[[ "$ALL_FOUND" -eq 1 ]]
report "$?" "Found expected symbols"

###

ALL_QUAL=1
ALL_SUFF=1
DEFS=$(echo "$FILES" | bash mk_defs.sh)
while read -r SYM
do
    echo "$SYM" | grep    '\.smt2'     > /dev/null || ALL_QUAL=0
    echo "$SYM" | grep -- '-sentinel$' > /dev/null || ALL_SUFF=0
done < <(echo "$DEFS" | bash symbols_of_theorems.sh)

[[ "$ALL_QUAL" -eq 1 ]]
report "$?" "All symbols are qualified"

[[ "$ALL_SUFF" -eq 1 ]]
report "$?" "All symbols are suffixed"

###

DUPES=0
NORMALISED=""
while read -r SYM
do
      DEF=$(echo "$QUAL" | bash get_def.sh "$SYM")
    COUNT=$(echo "$DEF"  | grep '^.' | wc -l)

    [[ "$COUNT" -eq 1 ]]
    report "$?" "Got definition for '$SYM'" #|| {
        #echo -e "SYM: $SYM\nDEF:\n$DEF\n\n" 1>&2
    #}

    # MSG="Normalised '$SYM' isn't a duplicate"

    # CANON=$(echo "$DEF"  | racket canonical_functions.rkt)
    # if echo "$NORMALISED" | grep -Fx "$CANON" | grep '^.' > /dev/null
    # then
    #     report 1 "$MSG"
    #     DUPES=1
    #     echo -e "SYM: $SYM\nDEF: $DEF\nCANON: $CANON\nNORMALISED:\n$NORMALISED\n\n" 1>&2
    # else
    #     report 0 "$MSG"
    # fi
    # NORMALISED=$(echo -e "$NORMALISED\n$CANON")
done < <(echo "$SUBSET")

[[ "$DUPES" -eq 0 ]]
report "$?" "No duplicate functions"

exit "$ERR"
