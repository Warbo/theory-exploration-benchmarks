#!/usr/bin/env racket
#lang racket

(require racket/function)
(require racket/include)
(include "defs.rkt")

(define (tag-constructors x)
  ;; Tag constructors with 'CON' to disambiguate
  x)

(define (prefix-name n p)
  (string->symbol (string-append p (symbol->string n))))

(define (arg-decs-for c x)
  ;; Look through x for any definitions of c, and return its argument list
  (match x
    [(list 'declare-datatypes _ decs) (apply append (map (curry arg-decs-for-ty c) decs))]
    [(cons h t) (append (arg-decs-for c h)
                        (arg-decs-for c t))]
    [_ '()]))

(define (arg-decs-for-ty c x)
  (match x
    [(cons _ constructors) (apply append (map (curry arg-decs-for-con c) constructors))]))

(define (arg-decs-for-con c x)
  (match x
    [(list name) (if (equal? name c)
                     (list '())
                     '())]
    [(cons name args) (if (equal? name c)
                          (list args)
                          '())]))

(define (arg-apps-for c x)
  (map car (car (arg-decs-for c x))))

(define (constructor-type c x)
  (match x
    [(list 'declare-datatypes _ decs) (apply append (map (curry constructor-type-ty c) decs))]
    [(cons a b) (append (constructor-type c a) (constructor-type c b))]
    [_ '()]))

(define (concat-map f xs)
  (apply append (map f xs)))

(define (constructor-type-ty c dec)
  (concat-map (lambda (con)
                (if (equal? (car con) c)
                    (list (car dec))
                    '()))
              (cdr dec)))

(define (func-for x c)
  (let ([arg-decs (car (arg-decs-for c x))])
    (prefix-locals
     `(define-fun
        ,(prefix-name c "constructor")
        ,arg-decs
        ,(car (constructor-type c x))
        ,(if (empty? arg-decs)
             c
             (cons c (arg-apps-for c x)))))))

(define (add-constructor-funcs x)
  ;; Adding function for each constructor
  (let* ([consts (expression-constructors x)])
    (append x (map (curry func-for x) consts))))

(define (tag-types x)
  ;; Tag types with 'TYP' to disambiguate
  x)

(show
 (add-constructor-funcs
  (read-benchmark (port->string (current-input-port)))))
