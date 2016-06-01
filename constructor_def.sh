#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

function typeDef {
    racket constructor_def.rkt
}

function customConstructors {
    typeDef | removeDupes
}

function removeDupes {
    # Remove those definitions which we've renamed
    grep -vf removes

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

function renamedConstructors {
    while read -r LINE
    do
        if REPLACE=$(cut -f1 < renames | grep "^$LINE\t")
        then
            echo "$REPLACE" | cut -f2
        fi
    done
}

INPUT=$(cat)

 CUSTOM=$(echo "$INPUT" | customConstructors)
BUILTIN=$(echo "$INPUT" | builtInConstructors)
RENAMED=$(echo "$INPUT" | renamedConstructors)

echo -e "$CUSTOM\n$BUILTIN\n$RENAMED" | grep '^.' | sort -u
