#!/usr/bin/env racket
#lang racket

(require (file "defs.rkt"))
(get-fun-def (vector-ref (current-command-line-arguments) 0))
