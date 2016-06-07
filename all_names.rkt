#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define given-defs
  (map read-benchmark (port->lines (current-input-port))))

(define found-names
  (map (lambda (d)
         (join-spaces (names-in d)))
       given-defs))

(show found-names)

(exit)
(show (apply append (map names-in given-defs)))
