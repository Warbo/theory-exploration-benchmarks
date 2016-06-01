#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib

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

# Try making a signature with the Nat type, as well as its constructors. These
# shouldn't conflict, since duplicate definitions should be removed
echo -e "Nat\nS\nZ" | make_sig
report "$?" "Made signature of Nat, Z and S"

# Try making a signature of a few random symbols

 ALL=$(./all_symbols.sh)
SYMS=$(echo "$ALL" | shuf | head -n3)

ALL_MSG="All symbols form signature"

echo "$SYMS" | make_sig
if report "$?" "Made Haskell for $SYMS"
then
    # OK, maybe worth trying all symbols
    echo "$ALL" | make_sig

    if report "$?" "$ALL_MSG"
    then
        true
    else
        # Try to narrow-down to a broken sub-set of symbols
        SUBSET=$(echo "$ALL" | binary_search)
        echo -e "Can't make these symbols into a signature:\n$SUBSET" 1>&2
    fi
else
    # A sub-set didn't work, so the whole lot won't
    report 1 "$ALL_MSG"
fi

#HS=$( | make_sig)
#report "$?" "Made Haskell of all signatures"

exit "$ERR"
