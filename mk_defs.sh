#!/usr/bin/env bash

QUAL=$(./qual_all.sh)

echo -e "qual_all.sh output:\n$QUAL\n\n" 1>&2

echo "$QUAL" | ./norm_defs.sh
