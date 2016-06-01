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

function function_def {
    FD_IN=$(cat)
    echo "$FD_IN" | grep -F "(define-fun $1 "
    echo "$FD_IN" | grep    "(define-fun (par ([^)]*) ($1 "
    echo "$FD_IN" | grep -F "(define-fun-rec $1 "
    echo "$FD_IN" | grep    "(define-fun-rec (par ([^)]*) ($1 "
    echo "$FD_IN" | grep    "(define-funs-rec" | while read -r REC_LINE
    do
        if echo "$REC_LINE" | ./rec_names.rkt | grep -Fx "$1" > /dev/null
        then
            echo "$REC_LINE"
        fi
    done
}

function path {
    # Prefix our argument with the benchmarks directory, to save us typing it
    # over and over
    echo "modules/tip-benchmarks/benchmarks/$1"
}

function haveDef {
    DEF=$(echo "$DEFS" | function_def "$1")
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

exit

FILES=$(find modules/tip-benchmarks/benchmarks -name "*.smt2" | head -n20)

DEFS=$(echo "$FILES" | ./mk_defs.sh)

echo "$DEFS" | symbols_of_theorems.rkt

report "$?" "No duplicate functions"

exit "$ERR"
