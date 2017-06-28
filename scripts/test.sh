#!/usr/bin/env bash
BASE=$(dirname "$0")
raco test "$BASE/test.rkt"
