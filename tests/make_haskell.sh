#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib -p mysql

ERR=0

DIR="modules/tip-benchmarks/benchmarks"

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

function stringToHaskell {
    # mk_haskell.sh takes in filenames, so it can qualify names. This makes and
    # cleans up temporary files for testing.

    # Note: We make a file in a directory, to avoid problems if tmpdir begins
    # with a number (e.g. '/var/run/user/1000'); otherwise qualified variable
    # names would be invalid
    TEMP_DIR=$(mktemp --tmpdir -d "te-benchmark-temp-test-data-XXXXX")
    TEMP_FILE="$TEMP_DIR/test.smt2"

    echo "$1" | tr -d '\n' > "$TEMP_FILE"

    echo "$TEMP_FILE" | bash mk_haskell.sh
    STH_CODE="$?"

    rm -f "$TEMP_FILE"
    rmdir "$TEMP_DIR"
    return "$STH_CODE"
}

function testForm {
    FORM="(declare-datatypes ()
            ((Form (& (&_0 Form) (&_1 Form))
                   (Not (Not_0 Form))
                   (Var (Var_0 Int)))))"

    PREPARED=$(echo "$FORM" | tr -d '\n' | ./prepare.sh)
    report "$?" "Can prepare 'Form' input"

    echo "$PREPARED" | grep "(declare-datatypes" > /dev/null
    report "$?" "Prepared 'Form' input defines a datatype"

    SIG=$(stringToHaskell "$FORM")
    report "$?" "'Form' datatype gets a signature"

    DATA_COUNT=$(echo "$SIG" | grep -c "^data ")

    [[ "$DATA_COUNT" -eq 1 ]]
    report "$?" "'Form' datatype appears in signature" || {
        echo -e "SIG:\n$SIG\n\n" 1>&2
    }
}

function testMutualRecursion {
    MUT="(define-funs-rec
           ((models  ((x Bool)
                      (y Int))
                     Bool)
            (models2 ((q Bool)
                      (x Int))
                     Bool)
            (models5 ((q Bool)
                      (x Int)
                      (y Int))
                     Bool))

           ((ite x
              (models2 (models x y) y)
              (models5 (models x y) y y))

            (ite q
              (models5 (models q x) x x)
              (models2 (models q x) x))

            (ite q
              (models2 q x)
              (models5 q x y))))"

    PREPARED=$(echo "$MUT" | tr -d '\n' | ./prepare.sh)
    report "$?" "Can prepare mutually-recursive input"

    echo "$PREPARED" | grep "(define-funs-rec" > /dev/null
    report "$?" "Mutually-recursive function definitions survive preparation"

    SIG=$(stringToHaskell "$MUT")
    report "$?" "Mutually-recursive functions gets a signature"

    for FUN in models models2 models5
    do
        DEF_COUNT=$(echo "$SIG" | grep -c "^$FUN ")

        [[ "$DEF_COUNT" -gt 0 ]]
        report "$?" "'$FUN' is defined in signature" || {
            echo -e "SIG:\n$SIG\n\n" 1>&2
        }
    done
}

function testSingleFiles {
    FILES="$DIR/isaplanner/prop_54.smt2
$DIR/tip2015/propositional_AndIdempotent.smt2
$DIR/isaplanner/prop_36.smt2
$DIR/tip2015/sort_MSortTDPermutes.smt2
$DIR/tip2015/tree_sort_SortPermutes'.smt2
$DIR/tip2015/sort_StoogeSort2Permutes.smt2"

    while read -r F
    do
        SIG=$(echo "$F" | bash mk_haskell.sh)
        report "$?" "Made Haskell file for '$F'" || {
            echo -e "F: $F\nSIG:\n$SIG\n\n" 1>&2
            exit 1
        }
    done < <(echo "$FILES")
}

function testMultipleFiles {
    for N in 1 3
    do
        FILES=$(find "$DIR" -name "*.smt2" | shuf | head -n$N)

        SIG=$(echo "$FILES" | bash mk_haskell.sh)
        report "$?" "Made Haskell for $N files" || {
            echo -e "FILES:\n$FILES\n\nSIG:\n$SIG\n\n" 1>&2
            break  # Not worth trying larger samples
        }
    done
}

testForm

testMutualRecursion

testSingleFiles

#testMultipleFiles

exit "$ERR"
