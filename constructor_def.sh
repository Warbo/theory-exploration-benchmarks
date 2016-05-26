#!/usr/bin/env bash

function customConstructors {
    ./type_def.rkt
}

function builtInConstructors {
    GOT=$(cat)
    if echo "$GOT" | grep -xF "true" > /dev/null
    then
        true
        #echo '(declare-datatypes () ((Bool (true) (false))))'
    fi

    if echo "$GOT" | grep -xF "false" > /dev/null
    then
        true
        #echo '(declare-datatypes () ((Bool (true) (false))))'
    fi
}

INPUT=$(cat)

CUSTOM=$(echo "$INPUT" | customConstructors)
BUILTIN=$(echo "$INPUT" | builtInConstructors)

echo -e "$CUSTOM\n$BUILTIN" | grep '^.' | sort -u
