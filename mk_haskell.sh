#!/usr/bin/env bash

if [[ "x$1" = "xall" ]] && [[ -n "$SMT_FILE" ]]
then
    [[ -e "$SMT_FILE" ]] || {
        echo "Given file '$SMT_FILE' doesn't exist" 1>&2
        exit 1
    }
    echo "Using file '$SMT_FILE'" 1>&2
    DEFS=$(cat "$SMT_FILE")
else
    DEFS=$(bash mk_final_defs.sh)
fi

echo -e "DEFS:\n$DEFS\n\n" 1>&2

echo "$DEFS" | bash mk_signature.sh
