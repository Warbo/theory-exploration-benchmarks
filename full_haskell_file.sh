#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket -p haskellPackages.tip-lib mysql

ALL=$(find modules/tip-benchmarks/benchmarks -name "*.smt2")

if  [[ -n "$FILES" ]]
then
    echo "Using given FILES" 1>&2
elif [[ -z "$1" ]]
then
    echo "No limit given, using all files" 1>&2
    FILES="$ALL"
else
    echo "Given limit of '$1' files" 1>&2
    FILES=$(echo "$ALL" | head -n$1)
fi

COUNT=$(echo "$FILES" | wc -l)

echo -e "Processing $COUNT files:" 1>&2

echo "$FILES" | bash mk_haskell.sh
