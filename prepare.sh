#!/usr/bin/env bash

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

function nameReplacements {
    # Unqualify any names which only have one definition. For example, given:
    #
    # (define-fun foo.smt2baz-sentinel  ...)
    # (define-fun foo.smt2quux-sentinel ...)
    # (define-fun bar.smt2quux-sentinel ...)
    #
    # We can unqualify 'foo.smt2baz-sentinel' to get 'baz', but we can't for
    # 'quux' since there are two distinct versions.
    NR_NAMES=$(racket rec_names.rkt)

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

removePrefices | removeSuffices | addCheckSat
