#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(show (foldl (lambda (path rest)
               (append (symbols-of-theorem path) rest))
             '()
             (theorem-files)))
