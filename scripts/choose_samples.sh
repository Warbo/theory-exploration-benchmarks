#!/usr/bin/env bash

function usage {
    echo "Deterministically sample names from BENCHMARK_SOURCE.

Usage: BENCHMARK_SOURCE=... $0 N M

Where N is the size of each sample and M is the number of samples to take.

Returns a 2D array of sampled names."
}

function prepare {
    # Prefixes each line of the input with its peppered SHA256
    while read -r NAME
    do
        # Skip blanks
        [[ -n "$NAME" ]] || continue

        SHA=$(pepper "$1" "$2" "$NAME")
        echo -e "$SHA\t$NAME"
    done
}

function pepper {
    printf "sample-size-%s-selection-round-%s-%s" "$1" "$2" "$3" |
        sha256sum | cut -d ' ' -f 1
}

function take {
    # Take $1,     strip hashes, quote names, form array
    head -n+"$1" | cut -f 2    | jq -R '.'  | jq -s '.'
}

function sample {
    echo "$ALL_NAMES"          |
        prepare "$SIZE" "$REP" | # Prefix hash to each line
        sort                   | # Sorting hashes acts as a repeatable shuffle
        take "$SIZE"             # Take the first SIZE names
}

function samples {
    # Repeat for each sample
    for REP in $(seq 1 "$REPS")
    do
        sample
    done
}

# Check how we're called
if [[ "$#" -lt 2 ]]
then
    usage
    exit 1
fi

if [[ -z "$BENCHMARK_SOURCE" ]]
then
    usage
    exit 1
fi

# Read params
SIZE="$1"
REPS="$2"

# Read all "globalXXX" names from input and dedupe
ALL_NAMES=$(grep -o "global[0-9a-f][0-9a-f]*" < "$BENCHMARK_SOURCE" | sort -u)

samples | jq -s '.'
