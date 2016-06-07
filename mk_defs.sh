#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket pv

QUAL=$(bash qual_all.sh)

echo -e "qual_all.sh output:\n$QUAL\n\n" 1>&2

echo "$QUAL" | bash norm_defs.sh
