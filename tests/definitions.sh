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

SUB_DIRS=1
MODS_FOUND=1
QUALIFIED=1
EXPOSED=1
CONTENT=1
for DIR in "$D"/*
do
    DIR_NAME=$(basename "$DIR")
    INIT=$(echo "$DIR_NAME" | cut -c 1 | tr '[:lower:]' '[:upper:]')
    REST=$(echo "$DIR_NAME" | cut -c 2-)
    DST_NAME="${INIT}${REST}"

    SUB_DIR="benchmark_package/src/${DST_NAME}"
    [[ -d "$SUB_DIR" ]] || {
        SUB_DIRS=0
        echo -e "SUB_DIR '$SUB_DIR' not found" 1>&2
    }

    for MOD in "$DIR"/*.hs
    do
        MOD_NAME=$(basename "$MOD")
        DST_MOD="${SUB_DIR}/${MOD_NAME}"

        [[ -e "$DST_MOD" ]] || {
            MODS_FOUND=0
            echo -e "Couldn't find module '$DST_MOD'" 1>&2
        }

        QUAL=$(basename "${DST_NAME}.${MOD_NAME}" ".hs")
        grep "module $QUAL where" < "$DST_MOD" > /dev/null || {
            QUALIFIED=0
            echo -e "'$DST_MOD' doesn't have module name '$QUAL'" 1>&2
        }

        grep "exposed-modules:" < benchmark_package/benchmark-package.cabal |
                                  grep "$QUAL" > /dev/null || {
            EXPOSED=0
            echo -e "Module '$QUAL' is not exposed" 1>&2
        }

        SRC_BODY=$(grep -v "^module " < "$MOD"     | grep -v "^import ")
        DST_BODY=$(grep -v "^module " < "$DST_MOD" | grep -v "^import ")
        [[ "x$SRC_BODY" = "x$DST_BODY" ]] || {
            CONTENT=0
            echo -e "'$DST_MOD' content doesn't matche '$MOD'" 1>&2
        }
    done
done

[[ "$SUB_DIRS" -eq 1 ]]
report "$?" "Sub-directories exist"

[[ "$MODS_FOUND" -eq 1 ]]
report "$?" "Modules found"

[[ "$QUALIFIED" -eq 1 ]]
report "$?" "Module names are qualified"

[[ "$EXPOSED" -eq 1 ]]
report "$?" "Modules are exposed"

[[ "$CONTENT" -eq 1 ]]
report "$?" "Module content matches source"

pushd benchmark_package > /dev/null

if command -v hsConfig > /dev/null
then
    CFG=$(hsConfig 2>&1)
else
    CFG=$(cabal configure 2>&1)
fi
report "$?" "Can configure benchmark package" ||
    echo -e "Configure output:\n$CFG\n\n" 1>&2

BLD=$(cabal build 2>&1)
report "$?" "Can build benchmark package" ||
    echo -e "Build output:\n$BLD\n\n" 1>&2

popd > /dev/null

exit "$ERR"
