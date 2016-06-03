#! /usr/bin/env nix-shell
#! nix-shell -i bash -p racket

# Combine all definitions from modules/tip-benchmarks/benchmarks

find modules/tip-benchmarks/benchmarks -name "*.smt2" | bash mk_defs.sh
