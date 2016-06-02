#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

# Combine all definitions in files given on stdin

while read -r FILE
do
    NAME=$(basename "$FILE")
     DIR=$(basename "$(dirname "$FILE")")
    bash qual_defs.sh "$DIR/$NAME" < "$FILE"
done | ./norm_defs.sh
