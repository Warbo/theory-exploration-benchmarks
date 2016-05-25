#! /usr/bin/env nix-shell
#! nix-shell -p racket -i bash

./all_symbols.rkt | sort -u
