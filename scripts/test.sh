#!/usr/bin/env bash
BASE=$(dirname "$0")
IN_TEST=1 raco test "$BASE/test.rkt"
