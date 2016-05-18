#!/usr/bin/env bash

SRC="modules/tip-benchmarks/original"
[[ -d "$SRC" ]] || {
    echo "Couldn't find '$SRC'" 1>&2
    exit 1
}

DST="benchmark_package/src"

for DIR in "$SRC"/*
do
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

    for MOD in "$DIR"/*.hs
    do
        MOD_NAME=$(basename "$MOD")

        sed -e "s/^module \([^ ]*\) /module $DST_NAME.\1 /g" < "$MOD" \
                                                             > "$QUAL"/"$MOD_NAME"
    done
done
