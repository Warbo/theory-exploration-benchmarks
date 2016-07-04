#!/usr/bin/env racket
#lang racket

(require racket/include)
(require racket/trace)
(include "defs.rkt")

(define given-functions
  (filter (lambda (x)
            (not (eof-object? x)))
          (map (lambda (line)
                 (with-input-from-string line
                   read))
               (port->lines (current-input-port)))))

(define (normalise def)
  (match def
    [(list 'define-fun
           (list 'par p
                 (list name args return body))) (list 'define-fun
                                                      (list 'par ))]
    [(list name args typ body) (mk-def name args typ (normalise-cases body))]
    [_                         (error "Function definition of unexpected form" def)]))

(define (normalise-cases exp)
  (match exp
         [(list 'case pat body) (list 'case
                                      (replace-pat pat body)
                                      (replace-case pat (normalise-cases body)))]
         [(cons a b)            (cons (normalise-cases a) (normalise-cases b))]
         [_                     exp]))

(define (replace-pat pat body)
  (match pat
         [(cons con args) (cons con
                                (get-normal-pat args (+ 1 (highest-var body))))]
         [con             con]))

(define (highest-var exp)
  (if (string? exp)
      (if (string-prefix? exp "var")
          (string->number (substring exp 3))
          -1)
      (match exp
             [(cons a b) (max (highest-var a) (highest-var b))]
             [_          -1])))

(define (replace-case pat body)
  (match pat
         [(list con)      body]
         [(cons con args) (replace-args args (+ 1 (highest-var body)) body)]
         [con             body]
         [_               (error "Unexpected case structure" pat)]))

(define (mk-def name args typ body)
  (list name
        (get-normal-args args 0)
        typ
        (replace-args (map car args) 0 body)))

(define (nth-arg n)
  (string-append "var" (number->string n)))

(define (replace-args args n body)
  (match args
         [(cons arg rest) (replace-args rest
                                        (+ 1 n)
                                        (replace-in-canon arg (nth-arg n) body))]
         [null            body]))

(define (replace-in-canon src dst exp)
  (if (equal? src exp)
      dst
      (match exp
             [(cons a b) (cons (replace-in-canon src dst a)
                               (replace-in-canon src dst b))]
             [_          exp])))

(define (get-normal-pat args n)
  (match args
         [(cons name rest) (cons (nth-arg n)
                                 (get-normal-pat rest (+ 1 n)))]
         [_                null]))

(define (get-normal-args args n)
  (match args
         [(cons (list name typ) rest) (cons (list (nth-arg n) typ)
                                            (get-normal-args rest (+ 1 n)))]
         [_                           null]))

(show (map norm given-functions))
