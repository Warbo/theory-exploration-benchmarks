#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
#lang racket

(require racket/include)
(include "defs.rkt")

(define (defs-from given exp)
  (match exp
         [(list 'define-fun     name args typ body) (if (equal? (symbol->string name)
                                                                given)
                                                        (list (list name
                                                                    args
                                                                    typ
                                                                    body))
                                                        null)]
         [(list 'define-fun-rec
                (list 'par p
                      (list name args typ body)))   (if (equal? (symbol->string name)
                                                                given)
                                                        (list (list name
                                                                    args
                                                                    typ
                                                                    body))
                                                        null)]
         [(list 'define-fun-rec name args typ body) (if (equal? (symbol->string name)
                                                                given)
                                                        (list (list name
                                                                    args
                                                                    typ
                                                                    body))
                                                        null)]
         [(cons a b)                  (append (defs-from given a)
                                              (defs-from given b))]
         [_                           null]))

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

(define (unique-defs given)
  (remove-duplicates (defs-of given)))

(show (foldl (lambda (sym rest)
               (append (unique-defs sym) rest))
             '()
             given-symbols))
