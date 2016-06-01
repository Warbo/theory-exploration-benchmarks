#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

function parts {
    grep '^(' | while read -r COMPOUND
    do
        echo "$COMPOUND" | sed -e 's/[()]/ /g'  |
                           sed -e 's/ [ ]*/ /g' |
                           tr ' ' '\n'
    done
}

function customDatatypes {
    racket type_def.rkt | removeDupes
}

function builtInDatatypes {
    if grep -xF "Bool" > /dev/null
    then
        true
        #echo '(declare-datatypes () ((Bool (true) (false))))'
    fi
}

function removeDupes {
    grep -vf removes
}

function renamedDatatypes {
    while read -r LINE
    do
        grep -F "$LINE" < renames | while read -r REP
        do
            F1=$(echo "$REP" | cut -f1)
            F2=$(echo "$REP" | cut -f2)
            if [[ "x$F1" = "x$LINE" ]]
            then
                echo "$F2"
            fi
        done
    done
}

RAW_INPUT=$(cat)
PARTS=$(echo "$RAW_INPUT" | parts)
INPUT=$(echo -e "$RAW_INPUT\n$PARTS" | grep '^.')

 CUSTOM=$(echo "$INPUT" | customDatatypes)
BUILTIN=$(echo "$INPUT" | builtInDatatypes)
RENAMED=$(echo "$INPUT" | renamedDatatypes)

echo -e "$CUSTOM\n$BUILTIN\n$RENAMED" | grep '^.' | sort -u
