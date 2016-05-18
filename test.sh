#!/usr/bin/env bash

ERR=0

for T in tests/*
do
    MSG="Successfully ran '$T'"
    if ./"$T"
    then
        echo "ok - $MSG"
    else
        ERR=1
        echo "not ok - $MSG"
    fi
done

MSG="Test suite completed"
if [[ "$ERR" -eq 0 ]]
then
    echo "ok - $MSG"
else
    echo "not ok - $MSG"
fi

exit "$ERR"
