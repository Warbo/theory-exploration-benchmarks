#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

# Combine all definitions in files given on stdin

function trim {
    grep -v "^(assert-not " |
    grep -v "^(check-sat)"
}

DEFS=""
while read -r FILE
do
    NAME=$(basename "$FILE")
     DIR=$(basename "$(dirname "$FILE")")
    NEW=$(NAME="$DIR/$NAME" racket qualify.rkt < "$FILE")
    DEFS=$(echo -e "$DEFS\n$NEW" | trim)
done

echo "$DEFS"
