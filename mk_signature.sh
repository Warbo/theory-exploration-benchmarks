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
    TO_DEFINE=$(echo -e "$NEW\n$TO_DEFINE" | sort -u | grep '^.')

    # Functions may call other functions, so look up their definitions
    FUNC_DEFS=$(echo "$TO_DEFINE" | ./function_def.sh)
    TYPES=$(echo "$FUNC_DEFS" | ./types_from_defs.rkt)

    # Look up symbols used in those definitions
     NEW_SYMS=$(echo "$FUNC_DEFS" | ./symbols_of_theorems.rkt | sort -u)
    NEW_TYPES=$(echo "$FUNC_DEFS" | ./types_from_defs.rkt     | sort -u)
    NEW=$(echo -e "$NEW_SYMS\n$NEW_TYPES" | sort -u | grep '^.')
done

FUNC_DEFS=$(echo "$TO_DEFINE" | ./function_def.sh)
CONS_DEFS=$(echo "$TO_DEFINE" | ./constructor_def.sh)
TYPE_DEFS=$(echo "$TO_DEFINE" | ./type_def.sh)

ALL_TYPES=$(echo -e "$CONS_DEFS\n$TYPE_DEFS" | sort -u)

echo -e "$ALL_TYPES\n$FUNC_DEFS\n(check-sat)"
