#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

racket function_def.rkt | grep '^.' | sort -u
