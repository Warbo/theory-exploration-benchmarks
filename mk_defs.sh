#!/usr/bin/env bash
set -e
set -o pipefail

./qual_all.rkt | ./norm_defs.sh
