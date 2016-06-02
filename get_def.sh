#!/usr/bin/env bash

INPUT=$(cat)

echo "$INPUT" | ./get_fun_def.sh "$1"
echo "$INPUT" | NAME="$1" ./get_con_def.rkt
