#!/usr/bin/env bash

# Return any lines of stdin which are function definitions of $1

INPUT=$(cat)
echo "$INPUT" | grep -F "(define-fun $1 "
echo "$INPUT" | grep    "(define-fun (par ([^)]*) ($1 "
echo "$INPUT" | grep -F "(define-fun-rec $1 "
echo "$INPUT" | grep    "(define-fun-rec (par ([^)]*) ($1 "
echo "$INPUT" | grep    "(define-funs-rec" | while read -r REC_LINE
do
    if echo "$REC_LINE" | racket rec_names.rkt | grep -Fx "$1" > /dev/null
    then
        echo "$REC_LINE"
    fi
done
