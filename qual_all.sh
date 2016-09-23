#!/usr/bin/env bash

function trim {
    grep -v "^(assert-not " |
        grep -v "^(check-sat)"
}

# Combine all definitions in files given on stdin

./qual_all.rkt | trim
