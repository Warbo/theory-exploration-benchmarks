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
  (map (lambda (dec) (list 'declare-datatypes given (list dec)))
       (filter (lambda (ty-dec)
                 (equal? (format "~a" (car ty-dec))
                         sym))
               ty-decs)))

(define given-symbols
  (port->lines (current-input-port)))

(define (files-with given)
  (filter (lambda (path)
            (member given (map (lambda (x) (format "~a" x))
                               (types-of-theorem path))))
          (theorem-files)))

(define (defs-of given)
  (foldl (lambda (path rest)
           (append (defs-from given (read-benchmark (file->string path)))
                   rest))
         '()
         (files-with given)))

(define (unique-defs-of given)
  (remove-duplicates (defs-of given)))

(show (foldl (lambda (sym rest)
               (append (unique-defs-of sym)
                       rest))
             '()
             given-symbols))
