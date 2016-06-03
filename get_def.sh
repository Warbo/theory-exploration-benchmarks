#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

INPUT=$(cat)

echo "$INPUT" | bash get_fun_def.sh "$1"
echo "$INPUT" | NAME="$1" racket get_con_def.rkt
