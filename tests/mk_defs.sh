#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket pv

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
tip2015/sort_StoogeSort2IsSort.smt2nil-sentinel
tip2015/sort_StoogeSort2IsSort.smt2cons-sentinel
tip2015/sort_StoogeSort2IsSort.smt2sort2-sentinel
tip2015/sort_StoogeSort2IsSort.smt2insert2-sentinel
tip2015/sort_StoogeSort2IsSort.smt2zsplitAt-sentinel
tip2015/sort_StoogeSort2IsSort.smt2ztake-sentinel
tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2-sentinel"

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
    echo "$SYM" | grep    '\.smt2'     > /dev/null || {
        ALL_QUAL=0
        echo -e "Unqualified symbol: $SYM" 1>&2
    }
    echo "$SYM" | grep -- '-sentinel$' > /dev/null || {
        ALL_SUFF=0
        echo -e "Unsuffixed symbol: $SYM" 1>&2
    }
done < <(echo "$DEFS" | bash symbols_of_theorems.sh | grep '^.')

[[ "$ALL_QUAL" -eq 1 ]]
report "$?" "All symbols are qualified"

[[ "$ALL_SUFF" -eq 1 ]]
report "$?" "All symbols are suffixed"

###

DUPES=0
NORMALISED=$(echo "$DEFS" | racket canonical_functions.rkt)
while read -r NORM
do
    COUNT=$(echo "$NORMALISED" | grep -cF "$NORM")
    [[ "$COUNT" -eq 1 ]] || {
        DUPES=1
        echo -e "Duplicate normalised forms!\nCOUNT: $COUNT\nNORM:$NORM" 1>&2
    }
done < <(echo "$NORMALISED")

[[ "$DUPES" -eq 0 ]]
report "$?" "No alpha-equivalent duplicates in result"

GOT_QUAL=1
INTACT=1
while read -r SYM
do
    printf '.' 1>&2
      DEF=$(echo "$QUAL" | bash get_def.sh "$SYM")
    COUNT=$(echo "$DEF"  | grep -c '^.')

    [[ "$COUNT" -eq 1 ]] || {
        GOT_QUAL=0
        echo -e "SYM: $SYM\nDEF:\n$DEF\n\n" 1>&2
    }

      NORM_DEF=$(echo "$DEFS"     | bash get_def.sh "$SYM")
    NORM_COUNT=$(echo "$NORM_DEF" | grep -c '^.')

    [[ "$NORM_COUNT" -lt 2 ]] || {
        echo "Got more than one definition of '$SYM'!" 1>&2
    }

    if [[ "$NORM_COUNT" -eq 1 ]]
    then
        # The symbols in NORM_DEF may be replacements, so we can't compare
        # directly. Instead, we just infer the structure:
         DEF_SHAPE=$(echo      "$DEF" | tr -dc '()')
        NORM_SHAPE=$(echo "$NORM_DEF" | tr -dc '()')

        [[ "x$DEF_SHAPE" = "x$NORM_SHAPE" ]] || {
            INTACT=0
            echo    "Mangled function definition!" 1>&2
            echo -e "DEF: $DEF\nNORM_DEF: $NORM_DEF" 1>&2
        }
    fi
done < <(echo "$SUBSET")

[[ "$GOT_QUAL" -eq 1 ]]
report "$?" "All expected symbols got qualified"

[[ "$INTACT" -eq 1 ]]
report "$?" "Duplicate removal keeps definitions intact"

exit "$ERR"
