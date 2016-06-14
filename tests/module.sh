#!/usr/bin/env bash

ERR=0

function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
        return 0
    else
        echo "not ok - $2"
        ERR=1
        return 1
    fi
}

function tearDown {
    rm -rf "$OUT_DIR"
}

trap tearDown EXIT

for N in 1 3 50
do
    echo "Testing with $N files" 1>&2

    FILES=$(find modules/tip-benchmarks/benchmarks/ -name "*.smt2" |
                   shuf                                            |
                   head -n$N)
    export FILES

    OUT_DIR=$(mktemp --tmpdir -d "te-benchmark-temp-test-module-XXXXX")
    export OUT_DIR

    ./full_haskell_package.sh "$N"
    report "$?" "Made Haskell from $N files" || {
        echo -e "FILES:\n$FILES\n\n" 1>&2
    }

    pushd "$OUT_DIR" > /dev/null

    [[ -d src ]]
    report "$?" "Made src directory"

    [[ -f src/A.hs ]]
    report "$?" "Made A.hs module"

    [[ -f tip-benchmark-sig.cabal ]]
    report "$?" "Made Cabal file"

    command -v cabal > /dev/null
    report "$?" "Have cabal"

    if command -v hsConfig > /dev/null
    then
        hsConfig
    else
        cabal configure
    fi
    report "$?" "Configured package"

    OUT=$(echo -e 'import A\n:browse' | cabal repl -v0)
    report "$?" "Ran Haskell"

    # If the import fails, we're stuck with the Prelude, which contains classes
    ! echo "$OUT" | grep "class Functor"
    report "$?" "Module imported successfully" || {
        echo -e "OUT:\n$OUT\n\nA.hs:" 1>&2
        cat src/A.hs
    }

    popd > /dev/null
    rm -rf "$OUT_DIR"
done

exit "$ERR"
