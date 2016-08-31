#!/usr/bin/env bash

INPUT=$(cat)

echo "$INPUT" | grep -F "$1" | ./get_fun_def.sh "$1"
echo "$INPUT" | grep -F "$1" | NAME="$1" ./get_con_def.rkt
