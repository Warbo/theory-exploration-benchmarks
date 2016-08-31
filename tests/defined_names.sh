#!/usr/bin/env bash

F="modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig3.smt2"

# A side-effect of qualifying all the definitions is that they end up on one
# line each, which is what we want
ONE_LINERS=$(echo "$F" | ./qual_all.sh)

# Look up the names in each definition individually
RESULT=""
while read -r LINE
do
    NAMES=$(echo "$LINE" | ./rec_names.rkt | tr '\n' ' ')
    RESULT=$(echo -e "$RESULT\n$NAMES" | grep '^.' | sed -e 's/[ ]*$//g')
done < <(echo "$ONE_LINERS")

# Look up the names in each definition all at once
ALL=$(echo "$ONE_LINERS" | ./all_names.rkt | grep '^.')

MSG="all_names.rkt output matches rec_names.rkt"

if [[ "x$RESULT" = "x$ALL" ]]
then
    echo "ok - $MSG"
    exit 0
else
    echo "not ok - $MSG"
    echo -e "NAMES:\n$RESULT\n\nALL:\n$ALL\n\n" 1>&2
    exit 1
fi
