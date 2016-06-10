#! /usr/bin/env nix-shell
#! nix-shell -p haskellPackages.tip-lib -p racket -i bash

INPUT=$(cat)

TIP_INPUT=$(echo "$INPUT" | bash prepare.sh)

tip <(echo -e "$TIP_INPUT") --haskell-spec
CODE="$?"

[[ "$CODE" -eq 0 ]] ||
    echo -e "INPUT:\n$INPUT\n\nTIP_INPUT:\n$TIP_INPUT\n\n" 1>&2

exit "$CODE"
