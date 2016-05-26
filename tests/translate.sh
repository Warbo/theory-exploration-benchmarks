#! /usr/bin/env nix-shell
#! nix-shell -p python -i bash

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

./all_symbols.sh | python ./sym_to_haskell.py
report "$?" "All symbols translate to Haskell"

exit "$ERR"
