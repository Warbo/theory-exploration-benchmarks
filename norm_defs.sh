#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

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

function findRedundanciesOld {
    # Find alpha-equivalent expressions in stdin, outputting entries for SO_FAR
    # and NAME_REPLACEMENTS

    THESE_SO_FAR="" # This variable is local to this sub-process

    # NOTE: We assume each line is a separate definition, and each definition
    # takes up only one line. Mutually-recursive definitions count as one.
    while read -r LINE
    do
        [[ -n "$LINE" ]] || continue

        # Get the normalised form of this line
        NORM=$(echo "$LINE" | racket canonical_functions.rkt)

        # Get the names defined in this line
        THESE_NAMES=$(echo "$LINE" | racket rec_names.rkt)

        # Check if this normal form has been seen before
        if EXISTING=$(echo "$THESE_SO_FAR" | grep -F "$NORM")
        then
            # We've seen an alpha-equivalent form before, so add it should be
            # removed from our don't include it in our
            # output. We now need to replace all references to this line's
            # definitions with those of the pre-existing definitions.

            # Since the definitions are alpha-equivalent, their names should come
            # out in the same order, so we just match them up 1:1

            # Note that no name will occur inside another, since they will be
            # qualified at the start, and end with "-sentinel". Hence it's safe to
            # do a string replace.

            echo "Found redundant line '$LINE'" 1>&2

            REPLACE_WITH=$(echo "$EXISTING" | cut -f1 | tr ' ' '\n')

            # Output entries for NAME_REPLACEMENTS
            while read -r REP
            do
                echo -e "NAME\t$REP"
            done < <(paste <(echo "$THESE_NAMES") <(echo "$REPLACE_WITH"))

            continue
        fi

        # Output a seen entry for SO_FAR and remember it locally too
         SPACE_NAMES=$(echo "$THESE_NAMES" | tr '\n' ' ')
        THESE_SO_FAR=$(echo -e "$THESE_SO_FAR\n$SPACE_NAMES\t$NORM")
        echo -e "DEF\t$LINE\t$NORM"
    done
}

function stripRedundancies {
    # Remove alpha-equivalent expressions from stdin, appending to SO_FAR and
    # NAME_REPLACEMENTS as necessary

    while read -r LINE
    do
        KEEP=1
        while read -r DEF_NAME
        do
            if FOUND_NAME=$(echo "$NAME_REPLACEMENTS" | grep '^.' |
                                                        cut -f2   |
                                                        grep "$DEF_NAME")
            then
                KEEP=0
                break
            fi
        done < <(echo "$LINE" | racket rec_names.rkt | grep '^.')
        [[ "$KEEP" -eq 0 ]] || echo "$LINE"
    done
}

function replaceReferences {
    while read -r LINE
    do
        while read -r REPLACEMENT
        do
            SRC=$(echo "$REPLACEMENT" | cut -f2)
            DST=$(echo "$REPLACEMENT" | cut -f3)

            PRE="$LINE"
            LINE="${LINE//"$SRC"/"$DST"}"
        done < <(echo "$NAME_REPLACEMENTS" | grep '^.')
        echo "$LINE"
    done
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

    # Remove the definitions of anything in NAME_REPLACEMENTS
    NORMALISED=$(echo "$NORMALISED" | stripRedundancies)

    # Replace all references to things which have been removed
    NORMALISED=$(echo "$NORMALISED" | replaceReferences)
done

echo "$NORMALISED"
