#!/usr/bin/env racket
#lang racket

(require (file "defs.rkt"))
(apply replace-strings (current-command-line-arguments))
