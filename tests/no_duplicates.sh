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
for SYM in Pos Neg Z S P N
do
    FOUND=$(echo "$SYM" | ./constructor_def.rkt)
    [[ -n "$FOUND" ]]
    report "$?" "Found at least one definition of '$SYM'"

    COUNT=$(echo "$FOUND" | wc -l)
    [[ "$COUNT" -eq 1 ]]
    report "$?" "Found exactly one definition of '$SYM'"
done

# Check we can find unique definitions for some known functions
for SYM in toInteger sign plus2 opposite timesSign mult minus plus absVal times
do
    FOUND=$(echo "$SYM" | ./function_def.sh)
    [[ -n "$FOUND" ]]
    report "$?" "Found at least one definition of '$SYM'"

    COUNT=$(echo "$FOUND" | wc -l)
    [[ "$COUNT" -eq 1 ]]
    report "$?" "Found exactly one definition of '$SYM'"
done

# Check we find unique definitions for all discovered functions and constructors

function symDef {
    # Not perfect, but tries to get a particular symbol's definition. For
    # example, if we're given "foo", we don't want the definition of "foobar",
    # and we don't want other definitions whose bodies call "foo"
    grep -F "$1" | grep "^($SYM "
}

ALL=$(./all_symbols.sh)

FUNS=$(echo "$ALL" | ./function_def.sh)
CONS=$(echo "$ALL" | ./constructor_def.rkt)

echo "$ALL" | while read -r SYM
do
    DEFS=$(echo -e "$FUNS\n$CONS" | symDef "$SYM")
    report "$?" "Found a definition for '$SYM'"

    COUNT=$(echo "$DEFS" | wc -l)

    [[ "$COUNT" -eq 1 ]]
    report "$?" "Definition of '$SYM' is unique"

    if [[ "$COUNT" -ne 1 ]]
    then
        echo -e "'$SYM' defs:\n$DEFS" 1>&2
    fi
done

# Check there's no overlap between function and constructor names
echo "$ALL" | while read -r SYM
do
    MSG=

    FUN="0"
    echo "$FUNS" | symDef "$SYM" > /dev/null && FUN="1"

    CON="0"
    echo "$CONS" | symDef "$SYM" > /dev/null && CON="1"

    ! [[ "$FUN$CON" = "11" ]]
    report "$?" "'$SYM' isn't both a function and a constructor"

    ! [[ "$FUN$CON" = "00" ]]
    report "$?" "'$SYM' is either a function xor a constructor"
done

exit "$ERR"
