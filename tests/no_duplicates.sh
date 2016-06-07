#!/usr/bin/env bash

ERR=0
function report {
    if [[ "$1" -eq 0 ]]
    then
        echo "ok - $2"
    else
        echo "not ok - $2"
        ERR=1
    fi
}

# Check we can get find unique definitions for some known constructors
GOT=1
UNIQUE=1
for SYM in Pos Neg Z S P N
do
    FOUND=$(echo "$SYM" | ./constructor_def.rkt)
    [[ -n "$FOUND" ]] || {
        GOT=0
        echo -e "No definition of '$SYM'" 1>&2
    }

    COUNT=$(echo "$FOUND" | wc -l)
    [[ "$COUNT" -eq 1 ]] || {
        UNIQUE=0
        echo -e "Multiple definitions of '$SYM'"
    }
done

[[ "$GOT" -eq 1 ]]
report "$?" "Found at least one definition of constructors"

[[ "$UNIQUE" -eq 1 ]]
report "$?" "Found exactly one definition of constructors"

# Check we can find unique definitions for some known functions
GOT=1
UNIQUE=1
for SYM in toInteger sign plus2 opposite timesSign mult minus plus absVal times
do
    FOUND=$(echo "$SYM" | ./function_def.sh)
    [[ -n "$FOUND" ]] || {
        GOT=0
        echo -e "No definition for '$SYM'" 1>&2
    }

    COUNT=$(echo "$FOUND" | wc -l)
    [[ "$COUNT" -eq 1 ]] || {
        UNIQUE=0
        echo -e "Multiple definitions of '$SYM'" 1>&2
    }
done

[[ "$GOT" -eq 1 ]]
report "$?" "Found at least one definition of functions"

[[ "$UNIQUE" -eq 1 ]]
report "$?" "Found exactly one definition of functions"

# Check we don't get parameters from types

LIST=$(echo "list"  | ./type_def.sh)

echo "$LIST" | grep "(a)" > /dev/null
report "$?" "Parameter 'a' appears in list definition"

SYMS=$(echo "$LIST" | ./symbols_of_theorems.rkt)

echo "$SYMS" | grep "nil" > /dev/null
report "$?" "Found nil in list definition"

echo "$SYMS" | grep "cons" > /dev/null
report "$?" "Found cons in list definition"

! echo "$SYMS" | grep "^a$" > /dev/null
report "$?" "Parameter 'a' ignored in list symbols"

TYPS=$(echo "$LIST" | ./types_from_defs.rkt)

echo "$TYPS" | grep "list" > /dev/null
report "$?" "Found list in list definition"

! echo "$TYPS" | grep "^a$" > /dev/null
report "$?" "Parameter 'a' ignored in list types"

# Check we find unique definitions for all discovered functions and constructors

function funDef {
    # Not perfect, but tries to get a particular symbol's definition. For
    # example, if we're given "foo", we don't want the definition of "foobar",
    # and we don't want other definitions whose bodies call "foo"
    grep -F "$1" | grep -F "($1 "
}

function conDef {
    # Not perfect, but tries to get a particular constructor's definition. For
    # example, if we're given "foo", we don't want the definition of "foobar",
    # and we don't want other definitions whose bodies call "foo"
    grep "^(declare-datatypes " | grep -F "$1" | grep -e "($1\( \|)\)"
}


ALL=$(./all_symbols.sh | shuf | head -n30)

FUNS=$(echo "$ALL" | ./function_def.sh)
CONS=$(echo "$ALL" | ./constructor_def.rkt)

GOT=1
UNIQUE=1
echo "$ALL" | while read -r SYM
do
    FUNDEFS=$(echo "$FUNS" | funDef "$SYM")
    CONDEFS=$(echo "$CONS" | conDef "$SYM")

    DEFS=$(echo -e "$FUNDEFS\n$CONDEFS" | grep '^.') || {
        GOT=0
        echo -e "No definition for '$SYM'" 1>&2
    }

    COUNT=$(echo "$DEFS" | wc -l)

    [[ "$COUNT" -eq 1 ]] || {
        UNIQUE=0
        echo -e "Multiple definitions of '$SYM':\n$DEFS\n\n" 1>&2
    }
done

[[ "$GOT" -eq 1 ]]
report "$?" "Found a definition for all symbols"

[[ "$UNIQUE" -eq 1 ]]
report "$?" "All symbol definitions are unique"

# Check there's no overlap between function and constructor names
echo "$ALL" | while read -r SYM
do
    FUN="0"
    echo "$FUNS" | funDef "$SYM" > /dev/null && FUN="1"

    CON="0"
    echo "$CONS" | conDef "$SYM" > /dev/null && CON="1"

    ! [[ "$FUN$CON" = "11" ]]
    report "$?" "'$SYM' isn't both a function and a constructor"

    [[ "$FUN$CON" = "11" ]] && {
        echo "Function definitions for '$SYM'"
        echo "$FUNS" | funDef "$SYM"
        echo "Constructor definitions for '$SYM'"
        echo "$CONS" | conDef "$SYM"
    } 1>&2

    ! [[ "$FUN$CON" = "00" ]]
    report "$?" "'$SYM' is either a function xor a constructor"

    [[ "$FUN$CON" = "00" ]] && {
        echo "Function definitions for '$SYM'"
        echo "$FUNS" | funDef "$SYM"
        echo "Constructor definitions for '$SYM'"
        echo "$CONS" | conDef "$SYM"
    } 1>&2
done

exit "$ERR"
