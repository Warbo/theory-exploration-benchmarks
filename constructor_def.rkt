#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (defs-from sym exp)
  (match exp
         [(list 'declare-datatypes given decs) (find-defs sym given decs)]
         [(cons a b)                           (append (defs-from sym a)
                                                       (defs-from sym b))]
         [_                                    null]))

(define (find-defs sym given ty-decs)
  (map (lambda (dec) (list given dec))
       (filter (lambda (ty-dec)
                 (any (lambda (con-dec)
                        (equal? (symbol->string (car con-dec))
                                sym))
                      (cdr ty-dec)))
               ty-decs)))

(define (any f xs)
  (match xs
         [(cons a b) (or (f a) (any f b))]
         [_          #f]))

(define given-symbols
  (port->lines (current-input-port)))

(define (files-with given)
  (filter (lambda (path)
            (member given (map symbol->string (symbols-of-theorem path))))
          (theorem-files)))

(define (defs-of given)
  (foldl (lambda (path rest)
           (append (defs-from given (read-benchmark (file->string path)))
                   rest))
         '()
         (files-with given)))

(define (unique-defs-of given)
  (remove-duplicates (defs-of given)))

(show (map unique-defs-of given-symbols))
