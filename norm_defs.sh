#!/usr/bin/env bash

# Remove alpha-equivalent definitions and update references

# This starts as the given input (minus any blank lines), and is iteratively
# deduped
NORMALISED=$(cat | grep '^.')

# This will store the contents of NORMALISED from the last iteration, so we can
# tell when the content has stabilised
OLD=""

# Maps names removed due to redundancy, to the equivalent included name, as
# "NAME\t$OLD\t$NEW"
NAME_REPLACEMENTS=""

function stripRedundancies {
    # Remove alpha-equivalent expressions from stdin, according to
    # NAME_REPLACEMENTS
    REPLACEMENTS=$(echo "$NAME_REPLACEMENTS" | grep '^.' | cut -f1)
    SR_INPUT=$(cat | grep '^.')

    SR_COUNT=$(echo "$SR_INPUT" | wc -l)
    SR_INDEX=1

    while read -r LINE_DEFS
    do
        echo "$SR_INDEX/$SR_COUNT" 1>&2
        SR_INDEX=$(( SR_INDEX + 1 ))

             KEEP=1
             LINE=$(echo "$LINE_DEFS" | cut -f1)
        DEF_NAMES=$(echo "$LINE_DEFS" | cut -f2)
        while read -r DEF_NAME
        do
            if echo "$REPLACEMENTS" | grep "$DEF_NAME" > /dev/null
            then
                KEEP=0
                break
            fi
        done < <(echo "$DEF_NAMES" | tr ' ' '\n' | grep '^.')
        [[ "$KEEP" -eq 0 ]] || echo "$LINE"
    done < <(paste <(echo "$SR_INPUT") <(echo "$SR_INPUT" | ./all_names.rkt))
}

function replaceReferences {
    # We use Python because Bash is SLOW
    ./replace_strings.py <(echo "$NAME_REPLACEMENTS" | grep '^.')
}

while ! [[ "x$NORMALISED" = "x$OLD" ]]
do
    OLD="$NORMALISED"

    # Find alpha-equivalent terms
    NAME_REPLACEMENTS=$(echo "$NORMALISED" | ./find_redundancies.rkt)
    COUNT=$(echo "$NAME_REPLACEMENTS" | wc -l)

    # Remove the definitions of anything in NAME_REPLACEMENTS
    echo "Removing $COUNT alpha-equivalent definitions" 1>&2
    NORMALISED=$(echo "$NORMALISED" | stripRedundancies)

    # Replace all references to things which have been removed
    echo "Replacing references to removed definitions" 1>&2
    NORMALISED=$(echo "$NORMALISED" | replaceReferences)
done

echo "$NORMALISED"
