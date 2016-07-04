#!/usr/bin/env racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define name
  (getenv "NAME"))

(show (remove-duplicates (defs-of-stdin name)))
