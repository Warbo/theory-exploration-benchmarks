#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

function addCheckSat {
    # Add '(check-sat)' as the last line to appease tip-tools
    cat
    echo "(check-sat)"
}

function removeSuffices {
    # Remove "-sentinel" suffices. Do this after all other string-based
    # transformations, since the sentinels prevent us messing with, say, the
    # symbol "plus2", when we only wanted to change the symbol "plus"
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
    # Remove problematic lines (e.g. symbols which don't have a corresponding
    # definition).
    # FIXME: We should really make these work rather than stripping them out
    fixInt | grep -v "[() ]or2[() ]" |
             grep -v "tip2015/propositional_AndCommutative.smt2models" |
             grep -v "\.smt2>" | # tip2015/sort_QSortPermutes.smt2>
             #grep -v "\.smt2[^a-zA-Z0-9_]" | # tip2015/sort_StoogeSort2Permutes.smt2*
             #grep -v "\.smt2Form" | # tip2015/propositional_AndIdempotent.smt2Form
             grep -v "tip2015/polyrec_seq_index.smt2mod"
}

function fixInt {
    # "Int" may be built-in, if so remove any qualification
    # FIXME: This should be handled by the qualify script. Try disabling this and see
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
    # Unqualify any names which only have one definition. For example, given:
    #
    # (define-fun foo.smt2baz-sentinel  ...)
    # (define-fun foo.smt2quux-sentinel ...)
    # (define-fun bar.smt2quux-sentinel ...)
    #
    # We can unqualify 'foo.smt2baz-sentinel' to get 'baz', but we can't for
    # 'quux' since there are two distinct versions.
    NR_NAMES=$(allNames)

    while read -r NAME
    do
        if echo "$NAME" | grep ".smt2" > /dev/null
        then
            UNQUAL=$(echo "$NAME" | sed -e 's/.*\.smt2\(.*\)/\1/g' |
                                    sed -e 's/-sentinel//g')
            COUNT=$(echo "$NR_NAMES" | grep -cF ".smt2$UNQUAL-sentinel")
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
