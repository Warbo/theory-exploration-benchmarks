#!/usr/bin/env bash

INPUT=$(cat)

echo "Running tip --haskell-spec" 1>&2
tip <(echo "$INPUT") --haskell-spec
CODE="$?"

[[ "$CODE" -eq 0 ]] ||
    echo -e "INPUT:\n$INPUT\n\n" 1>&2

exit "$CODE"
