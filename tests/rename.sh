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

DEFS=$(echo "S" | ./constructor_def.sh)
report "$?" "Got constructors for S"

COUNT=$(echo "$DEFS" | grep '^.' | wc -l)
[[ "$COUNT" -eq 1 ]]
report "$?" "Got one definition of S"

DEFS=$(echo "SType" | ./type_def.sh)
report "$?" "Got type for SType"

COUNT=$(echo "$DEFS" | grep '^.' | wc -l)
[[ "$COUNT" -eq 1 ]]
report "$?" "Got one definition of SType"

exit "$ERR"
