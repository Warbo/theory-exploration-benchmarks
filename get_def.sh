#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

INPUT=$(cat)

echo "$INPUT" | grep -F "$1" | bash get_fun_def.sh "$1"
echo "$INPUT" | grep -F "$1" | NAME="$1" racket get_con_def.rkt