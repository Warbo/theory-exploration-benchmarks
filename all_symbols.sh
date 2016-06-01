#! /usr/bin/env nix-shell
#! nix-shell -p racket -i bash

racket all_symbols.rkt | sort -u | grep '^.'
