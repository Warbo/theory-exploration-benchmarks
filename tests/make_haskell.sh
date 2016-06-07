#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib -p pv

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

# Try making a signature for a few files

for F in "$DIR/isaplanner/prop_54.smt2" \
         "$DIR/tip2015/propositional_AndIdempotent.smt2" \
         "$DIR/isaplanner/prop_36.smt2" \
         "$DIR/tip2015/sort_MSortTDPermutes.smt2" \
         "$DIR/tip2015/tree_sort_SortPermutes'.smt2"
do
    SIG=$(echo "$F" | bash mk_haskell.sh)
    report "$?" "Made Haskell file for '$F'" || {
        echo -e "F: $F\nSIG:\n$SIG\n\n" 1>&2
        exit 1
    }
done

# Try making a signature of a few random files

for N in 1 3
do
    FILES=$(find "$DIR" -name "*.smt2" | shuf | head -n$N)

    SIG=$(echo "$FILES" | bash mk_haskell.sh)
    report "$?" "Made Haskell for $N files" || {
        echo -e "FILES:\n$FILES\n\nSIG:\n$SIG\n\n" 1>&2
        break  # Not worth trying larger samples
    }
done

exit "$ERR"
