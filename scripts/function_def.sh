#!/usr/bin/env bash

./function_def.rkt | grep '^.' | sort -u
