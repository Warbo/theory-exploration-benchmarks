#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (names-in defs)
  (match defs
    [(list 'define-funs-rec decs defs)            (map car decs)]
    [(list 'define-fun
           (list 'par p
                 (list name args return body)))   (list name)]
    [(list 'define-fun name args return body)     (list name)]
    [(list 'define-fun-rec
           (list 'par p
                 (list name args return body)))   (list name)]
    [(list 'define-fun-rec name args return body) (list name)]
    [(list 'declare-datatypes given decs)         (type-names decs)]
    [(cons a b)                                   (append (names-in a) (names-in b))]
    [_                                            null]))

(define (type-names decs)
  (apply append (map (lambda (dec)
                       (cons (car dec) ; type name
                             (apply append (map (lambda (con)
                                                  (cons (car con) ; constructor name
                                                        (map car (cdr con)))) ; destructor names
                                                (cdr dec)))))
                     decs)))

(define given-defs
  (map read-benchmark (port->lines (current-input-port))))

(show (apply append (map names-in given-defs)))
