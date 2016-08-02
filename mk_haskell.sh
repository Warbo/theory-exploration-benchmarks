#!/usr/bin/env bash

if [[ "x$1" = "xall" ]] && command -v completeTipSig 2> /dev/null
then
    DEFS=$(cat "$(completeTipSig)")
else
    DEFS=$(bash mk_defs.sh)
fi

echo -e "DEFS:\n$DEFS\n\n" 1>&2

echo "$DEFS" | bash mk_signature.sh
