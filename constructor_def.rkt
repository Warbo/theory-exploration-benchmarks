#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (defs-from exp)
  (match exp
         [(list 'declare-datatypes given decs) (find-defs given decs)]
         [(cons a b)                           (append (defs-from a)
                                                       (defs-from b))]
         [_                                    null]))

(define (find-defs given ty-decs)
  (map (lambda (dec) (list given dec))
       (filter (lambda (ty-dec)
                 (any (lambda (con-dec)
                        (equal? (symbol->string (car con-dec))
                                given-symbol))
                      (cdr ty-dec)))
               ty-decs)))

(define (any f xs)
  (match xs
         [(cons a b) (or (f a) (any f b))]
         [_          #f]))

(define given-symbol
  (car (port->lines (current-input-port))))

(define files-with-given
  (filter (lambda (path)
            (member given-symbol (map symbol->string (symbols-of-theorem path))))
          (theorem-files)))

(define defs-of-given
  (foldl (lambda (path rest)
           (append (defs-from (read-benchmark (file->string path)))
                   rest))
         '()
         files-with-given))

(define unique-defs
  (remove-duplicates defs-of-given))

(define (show x)
  (displayln (format-symbols x)))

(show unique-defs)
