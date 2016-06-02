#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define name
  (getenv "NAME"))

(show (remove-duplicates (defs-of-stdin name)))
