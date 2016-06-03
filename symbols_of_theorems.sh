#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

racket symbols_of_theorems.rkt |
    grep -vFxf <(echo -e 'true-sentinel\nfalse-sentinel\nor-sentinel\nite-sentinel')
