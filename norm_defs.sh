#!/usr/bin/env bash

# Remove alpha-equivalent definitions and update references

# This starts as the given input (minus any blank lines), and is iteratively
# deduped
NORMALISED=$(cat | grep '^.')

# This will store the contents of NORMALISED from the last iteration, so we can
# tell when the content has stabilised
OLD=""

# Maps lines (definitions) to their normalised form, as "DEF\t$LINE\t$NORM"
SO_FAR=""

# Maps names removed due to redundancy, to the equivalent included name, as
# "NAME\t$OLD\t$NEW"
NAME_REPLACEMENTS=""

function findRedundancies {
    racket find_redundancies.rkt
}

function stripRedundancies {
    # Remove alpha-equivalent expressions from stdin, according to
    # NAME_REPLACEMENTS
    REPLACEMENTS=$(echo "$NAME_REPLACEMENTS" | grep '^.' | cut -f2)
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
    done < <(paste <(echo "$SR_INPUT") <(echo "$SR_INPUT" | racket all_names.rkt))
}

function replaceReferences {
    RR_INPUT=$(cat)
    RR_COUNT=$(echo "$NAME_REPLACEMENTS" | wc -l)
    RR_INDEX=1
    while IFS=$'\t' read sentinel source dest
    do
        echo "$RR_INDEX/$RR_COUNT" 1>&2
        RR_INDEX=$(( RR_INDEX + 1 ))

        RR_INPUT="${RR_INPUT//$source/$dest}"
    done < <(echo "$NAME_REPLACEMENTS" | grep '^.')

    echo "$RR_INPUT"
}

while ! [[ "x$NORMALISED" = "x$OLD" ]]
do
    OLD="$NORMALISED"

    # Find alpha-equivalent terms
    FINDINGS=$(echo "$NORMALISED" | findRedundancies)

    # Each "DEF" line indicates the first occurrence of a definition, which we
    # will keep while discarding any subsequent alpha-equivalent ones
               SO_FAR=$(echo "$FINDINGS" | grep "^DEF")

    # Each "NAME" line gives the name of something to remove, along with the
    # reference we should use in its place
    NAME_REPLACEMENTS=$(echo "$FINDINGS" | grep "^NAME")
    COUNT=$(echo "$NAME_REPLACEMENTS" | wc -l)

    # Remove the definitions of anything in NAME_REPLACEMENTS
    echo "Removing $COUNT alpha-equivalent definitions" 1>&2
    NORMALISED=$(echo "$NORMALISED" | stripRedundancies)

    # Replace all references to things which have been removed
    echo "Replacing references to removed definitions" 1>&2
    NORMALISED=$(echo "$NORMALISED" | replaceReferences)
done

echo "$NORMALISED"
