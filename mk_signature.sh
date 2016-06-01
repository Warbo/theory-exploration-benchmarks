#! /usr/bin/env nix-shell
#! nix-shell -p haskellPackages.tip-lib -p racket -i bash

INPUT=$(cat)
tip <(echo "$INPUT" | bash mk_closure.sh) --haskell
