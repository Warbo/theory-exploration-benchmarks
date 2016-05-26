#! /usr/bin/env nix-shell
#! nix-shell -p haskellPackages.tip-lib -i bash

# The symbols we're exploring in our signature
TO_EXPLORE=$(cat | grep '^.')

# The symbols we're exploring, as well as all required background functions
NEW=$(echo "$TO_EXPLORE" | sort)

TO_DEFINE=""

# Loop as long as NEW contains things not in TO_DEFINE
while diff --new-line-format="" --unchanged-line-format="" \
           <(echo "$NEW") <(echo "$TO_DEFINE") | grep '^.' 1>&2
do
    # Add the contents of NEW to TO_DEFINE
    TO_DEFINE=$(echo -e "$NEW\n$TO_DEFINE" | sort -u | grep '^.')

    # Functions may call other functions, so look up their definitions
    FUNC_DEFS=$(echo "$TO_DEFINE" | ./function_def.sh)
    TYPES=$(echo "$FUNC_DEFS" | ./types_from_defs.rkt)

    # Look up symbols used in those definitions
     NEW_SYMS=$(echo "$FUNC_DEFS" | ./symbols_of_theorems.rkt | sort -u)
    NEW_TYPES=$(echo "$FUNC_DEFS" | ./types_from_defs.rkt     | sort -u)
    NEW=$(echo -e "$NEW_SYMS\n$NEW_TYPES" | sort -u | grep '^.')
done

FUNC_DEFS=$(echo "$TO_DEFINE" | ./function_def.sh)
CONS_DEFS=$(echo "$TO_DEFINE" | ./constructor_def.sh)
TYPE_DEFS=$(echo "$TO_DEFINE" | ./type_def.sh)

ALL_TYPES=$(echo -e "$CONS_DEFS\n$TYPE_DEFS" | sort -u)

UNORDERED_DEFS=$(echo -e "$ALL_TYPES\n$FUNC_DEFS")

# Put definitions into dependency order

function defOf {
    # Get definition of $1 from stdin
    GIVEN=$(cat)
    echo "$GIVEN" | grep "(define-fun-rec $1 "
}

function nameOf {
    # Get the names of the functions defined in stdin
    GIVEN=$(cat)
    echo "$GIVEN" | sed -e 's/(define-fun-rec \([^ ]*\) .*/\1/g'
}

function lessThanEq {
    LTGIVEN=$(cat)
    if echo "$LTGIVEN" | defOf "$1" | grep -F "$2" > /dev/null
    then
        return 1
    else
        return 0
    fi
}

function orderDefs {
    GOT=$(cat)
    FUNCS=$(echo "$GOT" | grep    "(define-fun")
    TYPES=$(echo "$GOT" | grep -v "(define-fun")

    # Types don't seem to have any dependencies
    echo "$TYPES"

    echo "$FUNCS" | grep    "(define-fun-rec equal"
    echo "$FUNCS" | grep -v "(define-fun-rec equal"
}

DEFS=$(echo "$UNORDERED_DEFS" | orderDefs)

tip <(echo -e "$DEFS\n(check-sat)") --haskell
