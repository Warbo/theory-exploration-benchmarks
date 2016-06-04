#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib

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

function make_sig {
    bash mk_signature.sh
}

# Try making a signature involving the Nat type.

F="$DIR/isaplanner/prop_54.smt2"
SIG=$(echo "$F" | make_sig)
report "$?" "Made Haskell file for '$F'" ||
    echo -e "SIG:\n$SIG\n\n" 1>&2

# Try making a signature of a few random files

for N in 1 3 10 50
do
    FILES=$(find "$DIR" -name "*.smt2" | shuf | head -n$N)
     SYMS=$(echo "$FILES" | bash symbols_of_theorems.sh)

     ALL_MSG="All symbols form signature"

     SIG=$(echo "$SYMS" | make_sig)
     report "$?" "Made Haskell for $N files" || {
         echo -e "FILES:\n$FILES\n\nSYMS:\n$SYMS\n\nSIG:\n$SIG\n\n" 1>&2
         break  # Not worth trying larger samples
     }
done

exit "$ERR"
