#!/usr/bin/env bash

# Look up function definitions. Since some names denote multiple functions, we
# choose one arbitrarily, and give all the others unique names.

function toSwap {
    GOT=$(cat)
    #echo "$GOT" | grep "mult-with-plus2"
}

function removeSwaps {
    cat
}

function getSwappedDefs {
    cat
}

INPUT=$(cat)

SWAP=$(echo "$INPUT" | toSwap)

# Note that we don't want to invoke function_def.rkt for each entry individually
# since that goes really slowly

# Get regular definitions, but filter out those which require swapping
REGULAR=$(echo "$INPUT" | ./function_def.rkt | removeSwaps)

# Get any definitions which require swapping
SWAPPED=$(echo "$SWAP" | getSwappedDefs)

echo -e "$REGULAR\n$SWAPPED" | grep '^.' | sort -u
