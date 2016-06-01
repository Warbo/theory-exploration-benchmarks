#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib

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

function make_sig {
    bash mk_signature.sh
}

function split {
    # Split BROKEN into two halves: PRE and POST
    COUNT=$(echo "$BROKEN" | wc -l)
     HALF=$(( COUNT / 2 ))
    INDEX=$(( HALF  + 1 ))
    echo "Going from '$COUNT' to '$HALF'" 1>&2

     PRE=$(echo "$BROKEN" | head -n$HALF)
    POST=$(echo "$BROKEN" | tail -n +$INDEX)

    echo -e "(PRE\n$PRE\n)"   1>&2
    echo -e "(POST\n$POST\n)" 1>&2

    # Check PRE: if it's empty, or passes the tests, ignore it
    if [[ -z "$PRE" ]] || echo "$PRE" | make_sig
    then
        # Check POST: if it's empty, or passes the tests, stop
        if [[ -z "$POST" ]] || echo "$POST" | make_sig
        then
            echo "Neither half induces a failure, stopping" 1>&2
            return 1
        else
            echo "POST is non-empty and induces a failure, recursing" 1>&2
            BROKEN="$POST"
        fi
    else
        echo "PRE is non-empty and induces a failure, recursing" 1>&2
        BROKEN="$PRE"
    fi
    return 0
}

function binary_search {
       PRE=""
      POST=""
    BROKEN=$(cat)
    while true
    do
        echo -e "(BROKEN\n$BROKEN\n)" 1>&2

        # Stop if BROKEN only contains one symbol
        COUNT=$(echo "$BROKEN" | wc -l)
        [[ "$COUNT" -eq 1 ]] && break

        split || break
    done
    echo "$BROKEN"
}

ALL=$(./all_symbols.sh)

SYMS=$(echo "$ALL" | shuf | head -n3)

echo "$SYMS" | make_sig
report "$?" "Made Haskell for $SYMS"


exit "$ERR"

MSG="All symbols form signature"
if echo "$ALL" | make_sig
then
    report 0 "$MSG"
else
    report 1 "$MSG"

    SUBSET=$(echo "$ALL" | binary_search)
    echo -e "Can't make these symbols into a signature:\n$SUBSET" 1>&2
fi

#HS=$( | make_sig)
#report "$?" "Made Haskell of all signatures"
