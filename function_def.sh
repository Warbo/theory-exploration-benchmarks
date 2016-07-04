#!/usr/bin/env bash

racket function_def.rkt | grep '^.' | sort -u
