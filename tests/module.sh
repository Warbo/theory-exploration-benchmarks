#!/usr/bin/env bash

ERR=0

function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
    else
        ERR=1
        echo "not ok - $2"
    fi
}

cd benchmark_package || exit 1
report 0 "Package found"

hsConfig 1>&2 || exit 1
report 0 "Can configure"

cabal build 1>&2
report "$?" "Can build"

exit "$ERR"
