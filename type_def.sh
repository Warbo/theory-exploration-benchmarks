#!/usr/bin/env bash

function parts {
    grep '^(' | while read -r COMPOUND
    do
        echo "$COMPOUND" | sed -e 's/[()]/ /g'  |
                           sed -e 's/ [ ]*/ /g' |
                           tr ' ' '\n'
    done
}

function customDatatypes {
    ./type_def.rkt
}

function builtInDatatypes {
    if grep -xF "Bool" > /dev/null
    then
        echo '(declare-datatypes () ((Bool (true) (false))))'
    fi
}

RAW_INPUT=$(cat)
PARTS=$(echo "$RAW_INPUT" | parts)
INPUT=$(echo -e "$RAW_INPUT\n$PARTS" | grep '^.')

 CUSTOM=$(echo "$INPUT" | customDatatypes)
BUILTIN=$(echo "$INPUT" | builtInDatatypes)

echo -e "$CUSTOM\n$BUILTIN" | grep '^.' | sort -u
