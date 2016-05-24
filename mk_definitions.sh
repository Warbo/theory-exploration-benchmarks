#!/usr/bin/env bash

SRC="modules/tip-benchmarks/original"
[[ -d "$SRC" ]] || {
    echo "Couldn't find '$SRC'" 1>&2
    exit 1
}

DST="benchmark_package/src"

MOD_LINE=""

for DIR in "$SRC"/*
do
    # For each suite 'foo', make a module directory 'Foo'
    NAME=$(basename "$DIR")
    INIT=$(echo "$NAME" | cut -c 1 | tr '[:lower:]' '[:upper:]')
    REST=$(echo "$NAME" | cut -c 2-)
    DST_NAME="${INIT}${REST}"

    QUAL="${DST}/${DST_NAME}"
    if [[ -d "$QUAL" ]]
    then
        true
    else
        echo "Creating '$QUAL'" 1>&2
        mkdir -p "$QUAL"
    fi

    # For each module 'Bar.hs', make a copy 'Foo/Bar.hs'
    for MOD in "$DIR"/*.hs
    do
        MOD_NAME=$(basename "$MOD")

        # Prepend directory (e.g. 'Foo.') to module name
        MOD_CONTENT=$(cat "$MOD")
        MOD_QUAL=$(echo "$MOD_CONTENT" |
                   sed -e "s/^module \([^ ]*\) /module $DST_NAME.\1 /g")

        # Prepend directory (e.g. 'Foo.') to relevant imports
        for MOD2 in "$DIR"/*.hs
        do
            MOD2_NAME=$(basename "$MOD2" .hs)
            SEARCH="^import[ ]*${MOD2_NAME}\$"
            REPLACE="import ${DST_NAME}.${MOD2_NAME}"
            MOD_QUAL=$(echo "$MOD_QUAL" | sed -e "s/$SEARCH/$REPLACE/g")
        done

        echo "$MOD_QUAL" > "$QUAL"/"$MOD_NAME"

        HS_NAME=$(basename "$MOD" .hs)
        HS_MOD="${DST_NAME}.${HS_NAME}"
        MOD_LINE="$MOD_LINE, $HS_MOD"
    done
done

TRIM=$(echo "$MOD_LINE" | cut -d ',' -f 2-)
CABAL="benchmark_package/benchmark-package.cabal"
CABAL_DEFS=$(cat "$CABAL")

SEARCH='^[ ]*exposed-modules:.*'
REPLACE="  exposed-modules:$TRIM"
echo "$CABAL_DEFS" | sed -e "s/$SEARCH/$REPLACE/g" > "$CABAL"
