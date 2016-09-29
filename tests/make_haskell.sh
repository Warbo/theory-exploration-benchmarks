#!/usr/bin/env bash

ERR=0

DIR="modules/tip-benchmarks/benchmarks"

TEMP_DIR=$(mktemp --tmpdir -d "te-benchmark-temp-test-data-XXXXX")
DBG="$TEMP_DIR/stderr"

function tearDown {
    rm -rf "$TEMP_DIR"
}

trap tearDown EXIT

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

function stringToHaskell {
    # mk_final_defs.rkt takes in filenames, so it can qualify names. This makes
    # and cleans up temporary files for testing.

    # Note: We make a file in a directory, to avoid problems if tmpdir begins
    # with a number (e.g. '/var/run/user/1000'); otherwise qualified variable
    # names would be invalid
    TEMP_FILE="$TEMP_DIR/test.smt2"

    echo "$1" | tr -d '\n' > "$TEMP_FILE"

    echo "$TEMP_FILE" | ./mk_final_defs.rkt | ./mk_signature.sh
    STH_CODE="$?"

    rm -f "$TEMP_FILE"
    return "$STH_CODE"
}

exit "$ERR"
