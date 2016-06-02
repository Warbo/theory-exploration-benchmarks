#! /usr/bin/env nix-shell
#! nix-shell -p racket -i racket
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

(define (norm-func args body)
  (if (empty? args)
      (list args (norm body))
      (let* ([arg (car args)]
             [rec (norm-func (cdr args) body)]
             [v   (next-var rec)])
        (list (cons (cons v (cdr arg)) (car rec))
              (replace-in (car arg) v (cdr rec))))))

(define (norm-params ps def)
  (if (empty? ps)
      (match def
        [           (list name        args      return body)
         (let* ([fun (norm-func args body)])
           (list ps (list norm-func-1 (car fun) return (replace-in name
                                                                   norm-func-1
                                                                   (cadr fun)))))]
        [_ (error "Unexpected parameterised function definition" def)])
      (let* ([p   (car ps)]
             [rec (norm-params (cdr ps) def)]
             [v   (next-var rec)])
        (list (cons v (car rec))
              (replace-in p v (cdr rec))))))

(define var-prefix "normalise-var-")

(define (is-var? v)
  (if (symbol? v)
      (string-prefix? (symbol->string v) var-prefix)
      #f))

(define (var-num x)
  (string->number (substring (symbol->string x) (string-length var-prefix))))

(define (var-lt? x y)
  (< (var-num x) (var-num y)))

(define (max-var expr)
  (if (is-var? expr)
      expr
      (match expr
        [(cons a b) (if (var-lt? (max-var a) (max-var b))
                        (max-var b)
                        (max-var a))]
        [_ (string->symbol (string-append var-prefix "0"))])))

(define (next-var expr)
  (string->symbol
   (string-append var-prefix
                  (number->string (+ 1 (var-num (max-var expr)))))))

(define norm-func-prefix "defining-function-")

(define norm-func-1 (string->symbol (string-append norm-func-prefix "1")))

(define (norm expr)
  (match expr
    [(  list 'define-fun-rec (list 'par p         def))
     (let ([rec (norm-params p def)])
       (list 'define-fun-rec (list 'par (car rec) (cdr rec))))]

    [(  list 'define-fun-rec name        args      return body)
     (let ([rec       (norm-func args body)])
       (list 'define-fun-rec norm-func-1 (car rec) return (replace-in name
                                                                      norm-func-1
                                                                      (cdr rec))))]

    [(  list 'define-fun (list 'par p         def))
     (let ([rec (norm-params p def)])
       (list 'define-fun (list 'par (car rec) (cdr rec))))]

    [(  list 'define-fun name        args      return body)
     (let ([rec (norm-func args body)])
       (list 'define-fun norm-func-1 (car rec) return (replace-in name
                                                                  norm-func-1
                                                                  (cdr rec))))]

    [  (list 'declare-datatypes given     decs)
     (let* ([norm-decs (norm-types decs)]
            [rec       (norm-type-params given norm-decs)])
       (list 'declare-datatypes (car rec) (cdr rec)))]

    [(cons a b) (cons (norm a) (norm b))]

    [_ expr]))

(define (norm-type-params ps decs)
  (if (empty? ps)
      (list ps decs)
      (let* ([rec  (norm-type-params (cdr ps) decs)]
             [name (inc-name var-prefix (max-name var-prefix rec))])
        (list (cons name (first rec))
              (replace-in (car ps)
                          name
                          (second rec))))))

(define (norm-types decs)
  (if (empty? decs)
      decs
      (let ([rec (norm-types (cdr decs))])
        (cons (norm-type (car decs) rec) rec))))

(define (norm-type dec rest)
  (let ([name (inc-name type-prefix (max-name type-prefix rest))]
        [cs   (norm-constructors (cdr dec) rest)])
    (cons name (replace-in (car dec) name cs))))

(define type-prefix "defining-type-")

(define (name-num pre x)
  (if (symbol? x)
      (if (string-prefix? (symbol->string x) pre)
          (string->number (substring (symbol->string x)
                                     (string-length pre)))
          0)
      0))

(define (norm-constructors cs rest)
  (if (empty? cs)
      cs
      (let ([rec (norm-constructors (cdr cs) rest)])
        (cons (norm-constructor (car cs) (list rest rec)) rec))))

(define constructor-prefix "normalise-constructor-")
(define destructor-prefix  "normalise-destructor-")

(define (norm-constructor c rest)
  (let ([name (inc-name constructor-prefix (max-name constructor-prefix rest))])
    (cons name (norm-destructors (cdr c) rest))))

(define (norm-destructors ds rest)
  (if (empty? ds)
      ds
      (let* ([rec  (norm-destructors (cdr ds) rest)]
             [name (inc-name destructor-prefix (max-name destructor-prefix (list rest rec)))])
        (cons (cons name (cdr (car ds))) rec))))

(define (inc-name pre n)
  (string->symbol (string-append pre (number->string (+ 1 n)))))

(define (max-name pre expr)
  (match expr
    [(cons a b) (if (< (max-name pre a) (max-name pre b))
                    (max-name pre b)
                    (max-name pre a))]
    [x          (name-num pre x)]))

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
