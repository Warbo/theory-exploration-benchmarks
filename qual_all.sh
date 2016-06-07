#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

# Combine all definitions in files given on stdin

COUNT=0
while read -r FILE
do
    COUNT=$(( COUNT + 1 ))
    echo -e "$COUNT: Reading definitions from '$FILE'" 1>&2
    NAME=$(basename "$FILE")
    DIR=$(basename "$(dirname "$FILE")")
    bash qual_defs.sh "$DIR/$NAME" < "$FILE"
done
