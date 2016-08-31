#!/usr/bin/env bash

./all_symbols.rkt | sort -u | grep '^.'
