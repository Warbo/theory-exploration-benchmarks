#!/usr/bin/env racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (type-defs-from sym exp)
  (match exp
         [(list 'declare-datatypes given decs) (find-type-defs sym given decs)]
         [(cons a b)                           (append (type-defs-from sym a)
                                                       (type-defs-from sym b))]
         [_                                    null]))

(define (find-type-defs sym given ty-decs)
  (map (lambda (dec) (list 'declare-datatypes given (list dec)))
       (filter (lambda (ty-dec)
                 (equal? (format "~a" (car ty-dec))
                         sym))
               ty-decs)))

(define given-symbols
  (port->lines (current-input-port)))

(define (files-with-type given)
  (filter (lambda (path)
            (member given (map (lambda (x) (format "~a" x))
                               (types-of-theorem path))))
          (theorem-files)))

(define (defs-of-type given)
  (foldl (lambda (path rest)
           (append (type-defs-from given (read-benchmark (file->string path)))
                   rest))
         '()
         (files-with-type given)))

(define (unique-defs-of-type given)
  (remove-duplicates (defs-of-type given)))

(show (foldl (lambda (sym rest)
               (append (unique-defs-of-type sym)
                       rest))
             '()
             given-symbols))
