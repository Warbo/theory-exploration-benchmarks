#!/usr/bin/env bash

racket all_symbols.rkt | sort -u | grep '^.'
