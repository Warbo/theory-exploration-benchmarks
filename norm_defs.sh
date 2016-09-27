#!/usr/bin/env bash

# Remove alpha-equivalent definitions and update references

# This starts as the given input (minus any blank lines), and is iteratively
# deduped
NORMALISED=$(cat | grep '^.')

# This will store the contents of NORMALISED from the last iteration, so we can
# tell when the content has stabilised
OLD=""

while ! [[ "x$NORMALISED" = "x$OLD" ]]
do
    OLD="$NORMALISED"

    # Remove the definitions of anything in NAME_REPLACEMENTS
    NORMALISED=$(echo "$NORMALISED" | ./strip_redundancies.rkt)
done

echo "$NORMALISED"
