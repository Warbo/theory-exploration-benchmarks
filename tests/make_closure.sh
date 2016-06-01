#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

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

SEQ=$(echo "Seq" | bash mk_closure.sh)
report "$?" "mk_closure works on Seq"

echo "$SEQ" | grep "(declare-datatypes (a) ((Seq " > /dev/null
report "$?" "mk_closure includes Seq's definition"

echo "$SEQ" | grep "(declare-datatypes (a) ((Maybe " > /dev/null
report "$?" "mk_closure includes Maybe's definition"

echo "$SEQ" | grep "(declare-datatypes (a b) ((Pair " > /dev/null
report "$?" "mk_closure includes Pair's definition"

exit "$ERR"
