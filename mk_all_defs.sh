#!/usr/bin/env bash

# Combine all definitions from modules/tip-benchmarks/benchmarks

find modules/tip-benchmarks/benchmarks -name "*.smt2" | ./mk_defs.sh
