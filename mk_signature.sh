#!/usr/bin/env bash

if [ -t 0 ]
then
    echo "No input given (we're connected to a terminal)" 1>&2
    exit 1
fi
INPUT=$(cat)

echo "Running tip --haskell-spec" 1>&2
tip <(echo "$INPUT") --haskell-spec
CODE="$?"

[[ "$CODE" -eq 0 ]] ||
    echo -e "INPUT:\n$INPUT\n\n" 1>&2

exit "$CODE"
