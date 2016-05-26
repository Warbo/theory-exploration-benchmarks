#!/usr/bin/env bash

# The symbols we're exploring in our signature
TO_EXPLORE=$(cat | grep '^.')

# The symbols we're exploring, as well as all required background functions
NEW=$(echo "$TO_EXPLORE" | sort)

TO_DEFINE=""

# Loop as long as NEW contains things not in TO_DEFINE
while diff --new-line-format="" --unchanged-line-format="" \
           <(echo "$NEW") <(echo "$TO_DEFINE") | grep '^.' 1>&2
do
    # Add the contents of NEW to TO_DEFINE
    TO_DEFINE=$(echo -e "$TO_DEFINE\n$NEW" | sort | grep '^.')

    # Look up the definitions of everything in TO_DEFINE
    FUNC_DEFS=$(echo "$TO_DEFINE" | ./function_def.sh)
    CONS_DEFS=$(echo "$TO_DEFINE" | ./constructor_def.rkt)

    # Look up symbols used in those definitions
    NEW=$(echo "$FUNC_DEFS" | ./symbols_of_theorems.rkt | sort)
done

FUNC_DEFS=$(echo "$TO_DEFINE" | ./function_def.sh)
CONS_DEFS=$(echo "$TO_DEFINE" | ./constructor_def.rkt)

echo -e "$CONS_DEFS\n$FUNC_DEFS"
