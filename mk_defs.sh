#!/usr/bin/env bash
set -e
set -o pipefail

./qual_all.sh | ./norm_defs.sh
