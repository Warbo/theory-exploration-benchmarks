#!/usr/bin/env racket
#lang racket

(require racket/include)
(include "defs.rkt")

(show (foldl (lambda (path rest)
               (append (symbols-of-theorem path) rest))
             '()
             (theorem-files)))
