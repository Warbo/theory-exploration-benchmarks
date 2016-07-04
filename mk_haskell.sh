#!/usr/bin/env bash

DEFS=$(bash mk_defs.sh)

echo -e "mk_defs.sh output:\n$DEFS\n\n" 1>&2

echo "$DEFS" | bash mk_signature.sh
