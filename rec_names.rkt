#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (names-in defs)
  (match defs
    [(list 'define-funs-rec decs defs) (map car decs)]
    [(cons a b)                        (append (names-in a) (names-in b))]
    [_                                 null]))

(define given-defs
  (map read-benchmark (port->lines (current-input-port))))

(show (apply append (map names-in given-defs)))
