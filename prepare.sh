#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

function addCheckSat {
    # Add '(check-sat)' as the last line to appease tip-tools
    cat
    echo "(check-sat)"
}

function removeSuffices {
    # Remove "-sentinel" suffices
    sed -e 's/-sentinel//g'
}

function removePrefices {
    # Remove filename prefices when they result is unambiguous
    RP_INPUT=$(cat)
    while read -r RP_REPLACE
    do
        SRC=$(echo "$RP_REPLACE" | cut -f1)
        DST=$(echo "$RP_REPLACE" | cut -f2)
        RP_INPUT="${RP_INPUT//"$SRC"/"$DST"}"
    done < <(echo "$RP_INPUT" | nameReplacements)

    echo "$RP_INPUT"
}

function fixNames {
    fixInt
}

function fixInt {
    # "Int" may be built-in, if so remove any qualification
    FI_INPUT=$(cat)
    FI_OUTPUT="$FI_INPUT"

    FI_NAMES=$(echo "$FI_INPUT" | allNames)
    while read -r INT_NAME
    do
        if echo "$FI_NAMES" | grep -Fx "$INT_NAME" > /dev/null
        then
            # This name is defined. Leave it.
            true
        else
            # Undefined. Replace with native Int.
            FI_OUTPUT="${FI_OUTPUT//"$INT_NAME"/Int}"
        fi
    done < <(echo "$FI_INPUT" | racket types_from_defs.rkt | grep "\.smt2Int$")

    echo "$FI_OUTPUT"
}

function nameReplacements {
    NR_NAMES=$(allNames)

    while read -r NAME
    do
        if echo "$NAME" | grep ".smt2" > /dev/null
        then
            UNQUAL=$(echo "$NAME" | sed -e 's/.*\.smt2\(.*\)/\1/g' |
                                    sed -e 's/-sentinel//g')
            COUNT=$(echo "$NR_NAMES" | grep -cF "$UNQUAL")
            if [[ "$COUNT" -eq 1 ]]
            then
                echo -e "$NAME\t$UNQUAL"
            fi
        fi
    done < <(echo "$NR_NAMES")
}

function allNames {
    racket rec_names.rkt
}

removePrefices | removeSuffices | fixNames | addCheckSat