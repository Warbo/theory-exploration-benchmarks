#!/usr/bin/env bash

racket symbols_of_theorems.rkt |
    grep -vFxf <(echo -e 'true-sentinel\nfalse-sentinel\nor-sentinel\nite-sentinel')
