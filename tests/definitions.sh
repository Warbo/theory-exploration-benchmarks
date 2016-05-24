#!/usr/bin/env bash

ERR=0

function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
    else
        ERR=1
        echo "not ok - $2"
    fi
}

D="modules/tip-benchmarks/original"

[[ -d "$D" ]]
report "$?" "Directory '$D' exists"

for DIR in "$D"/*
do
    DIR_NAME=$(basename "$DIR")
    INIT=$(echo "$DIR_NAME" | cut -c 1 | tr '[:lower:]' '[:upper:]')
    REST=$(echo "$DIR_NAME" | cut -c 2-)
    DST_NAME="${INIT}${REST}"

    SUB_DIR="benchmark_package/src/${DST_NAME}"
    [[ -d "$SUB_DIR" ]]
    report "$?" "Directory '$SUB_DIR' exists"

    for MOD in "$DIR"/*.hs
    do
        MOD_NAME=$(basename "$MOD")
        DST_MOD="${SUB_DIR}/${MOD_NAME}"

        [[ -e "$DST_MOD" ]]
        report "$?" "Module '$DST_MOD' found"

        QUAL=$(basename "${DST_NAME}.${MOD_NAME}" ".hs")
        grep "module $QUAL where" < "$DST_MOD" > /dev/null
        report "$?" "'$DST_MOD' has module name '$QUAL'"

        grep "exposed-modules:" < benchmark_package/benchmark-package.cabal |
             grep "$QUAL" > /dev/null
        report "$?" "Module '$QUAL' is exposed"

        SRC_BODY=$(grep -v "^module " < "$MOD"     | grep -v "^import ")
        DST_BODY=$(grep -v "^module " < "$DST_MOD" | grep -v "^import ")
        [[ "x$SRC_BODY" = "x$DST_BODY" ]]
        report "$?" "'$DST_MOD' content matches '$MOD'"


    done
done

pushd benchmark_package > /dev/null
cabal build 1>&2
report "$?" "Can build benchmark package"
popd > /dev/null

exit "$ERR"
