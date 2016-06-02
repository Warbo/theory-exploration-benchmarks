#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

SO_FAR=""
while read -r LINE
do
    NORM=$(echo "$LINE" | racket canonical_functions.rkt)

    if EXISTING=$(echo "$SO_FAR" | grep -F "$NORM")
    then
        continue
    fi

    SO_FAR=$(echo -e "$SO_FAR\n$LINE\t$NORM")

    echo "$LINE"
done
