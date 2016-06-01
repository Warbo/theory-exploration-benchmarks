#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

function normalise {
    sort -u | grep '^.'
}

function symbols {
    SYMBOLS_INPUT=$(cat)
    SYMBOLS_FUNCS=$(echo "$SYMBOLS_INPUT" | racket symbols_of_theorems.rkt)
    SYMBOLS_TYPES=$(echo "$SYMBOLS_INPUT" | racket types_from_defs.rkt)
    echo -e "$SYMBOLS_FUNCS\n$SYMBOLS_TYPES" | normalise
}

function types {
    TYPES_INPUT=$(cat)
    TYPES_TYPES=$(echo "$TYPES_INPUT" | bash type_def.sh)
     TYPES_CONS=$(echo "$TYPES_INPUT" | bash constructor_def.sh)
    echo -e "$TYPES_TYPES\n$TYPES_CONS" | normalise
}

function functions {
    bash function_def.sh | normalise
}

# The symbols we're exploring in our signature
TO_EXPLORE=$(cat | normalise)

# The symbols we're exploring, as well as all required background functions
NEW=$(echo "$TO_EXPLORE" | sort)

TO_DEFINE=""

# Loop as long as NEW contains things not in TO_DEFINE
while DIFF=$(diff --new-line-format="" --unchanged-line-format="" \
                  <(echo "$NEW") <(echo "$TO_DEFINE") | grep '^.')
do
    echo -e "Unseen entries:\n$DIFF\n\n" 1>&2

    # Add the contents of NEW to TO_DEFINE
    TO_DEFINE=$(echo -e "$NEW\n$TO_DEFINE" | normalise)

    # Functions may call other functions, so look up their definitions
    FUNC_DEFS=$(echo "$TO_DEFINE" | functions)
        TYPES=$(echo "$TO_DEFINE" | types)

    DEFS=$(echo -e "$FUNC_DEFS\n$TYPES" | sort -u | grep '^.')

    # Look up symbols used in those functions
    NEW_SYMS=$(echo "$DEFS" | symbols)

    # Types may contain other types, so look up their definitions
    TYPE_DEFS=$(echo "$NEW_SYMS" | types)

    # Look up symbols used in those types
    TYPE_SYMS=$(echo "$TYPE_DEFS" | symbols)

    echo -e "FUNC_DEFS:\n$FUNC_DEFS\n\nNEW_TYPES:\n$NEW_TYPES\n\nTYPE_DEFS:\n$TYPE_DEFS\n\nTYPE_SYMS:\n$TYPE_SYMS\n\n" 1>&2

    # Combine all of these new symbols
    NEW=$(echo -e "$NEW_SYMS\n$NEW_TYPES\n$TYPE_SYMS" | sort -u | grep '^.')
done

FUNC_DEFS=$(echo "$TO_DEFINE" | functions)
CONS_DEFS=$(echo "$TO_DEFINE" | bash constructor_def.sh)
ALL_TYPES=$(echo "$TO_DEFINE" | types)

# Put definitions into dependency order

DEFS=""
TODO="$FUNC_DEFS"

for NAME in equal
do
    # Give the definition of NAME now, and remove it from REMAINING
     DEF=$(echo "$TODO" | grep    "(define-fun-rec $NAME ")
    TODO=$(echo "$TODO" | grep -v "(define-fun-rec $NAME ")
    DEFS=$(echo -e "$DEFS\n$DEF")
done

echo -e "$ALL_TYPES\n$DEFS\n(check-sat)"
