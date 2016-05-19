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

        SRC_BODY=$(grep -v "^module " < "$MOD")
        DST_BODY=$(grep -v "^module " < "$DST_MOD")
        [[ "x$SRC_BODY" = "x$DST_BODY" ]]
        report "$?" "'$DST_MOD' content matches '$MOD'"


    done
done

# function defs {
#     grep -r --no-filename "^[a-z][^=]*=" "$D" |
#         grep -v "|="                          |
#         grep -v "::.*=>"                      |
#         grep -v "^type "                      |
#         grep -v "^data "                      |
#         grep -v "^import "                    |
#         grep -v "^prop_"
# }

# function names {
#     defs | sed -e 's/ = .*//g'          |
#            sed -e 's/.*`\(.*\)`.*/\1/g' |  # x `infix` y
#            sed -e 's/.* \([^a-z_A-Z ][^a-z_A-Z ]*\) .*/\1/g'
# }

# names

exit "$ERR"
