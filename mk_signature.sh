#! /usr/bin/env nix-shell
#! nix-shell -p haskellPackages.tip-lib -p racket -i bash

CLOSURE=$(bash mk_closure.sh)
tip <(echo "$CLOSURE") --haskell
CODE="$?"

[[ "$CODE" -eq 0 ]] || echo -e "CLOSURE:\n$CLOSURE\n\n" 1>&2

exit "$CODE"
