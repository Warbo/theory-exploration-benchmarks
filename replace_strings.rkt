#!/usr/bin/env racket
#lang racket

(require (file "defs.rkt"))
(replace-strings (vector-ref (current-command-line-arguments) 0))
