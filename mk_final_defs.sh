#!/usr/bin/env bash
set -o pipefail
set -e

./mk_defs.rkt | ./prepare.rkt
