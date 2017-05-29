#lang racket

;; Various comparison functions

(provide arbitrary<=? lex<=? ss-eq? symbol<=?)

(module+ test
  (require rackunit))

;; Compare symbol names lexicographically
(define (symbol<=? x y)
  (string<=? (symbol->string x)
             (symbol->string y)))

;; Compare symbol names lexicographically
(define (symbol<? x y)
  (string<? (symbol->string x)
            (symbol->string y)))

;; Compare values, lexicographically on their displayed output
(define (arbitrary<=? x y)
  (string<=? (~a x) (~a y)))

;; Lexicographic comparison of two structures. We only focus on nested lists of
;; symbols.
(define (lex<=? x y)
  (cond
   ;; Compare symbols lexicographically
   [(and (symbol? x) (symbol? y)) (symbol<=? x y)]

   ;; Symbols are smaller than other structures
   [(symbol? x) #t]
   [(symbol? y) #f]

   ;; Numbers are the second-smallest type (only Reals, due to <='s contract).
   [(and (real? x) (real? y)) (<= x y)]
   [(real? x) #t]
   [(real? y) #f]

   ;; Strings are next
   [(and (string? x) (string? y)) (string<=? x y)]
   [(string? x) #t]
   [(string? y) #f]

   ;; If they're not symbols or strings, they must be lists
   [(not (list? x)) (error (format "Expected list, got ~s" x))]
   [(not (list? y)) (error (format "Expected list, got ~s" y))]

   ;; Empty lists are the smallest lists
   [(empty? x) #t]
   [(empty? y) #f]

   ;; Recurse on first elements; if not <=, they can't be equal wither, so stop.
   [(not (lex<=? (first x) (first y))) #f]

   ;; (first x) <= (first y), but are they equal? If not symmetric, we can stop.
   [(not (lex<=? (first y) (first x))) #t]

   ;; First elements are equal, recurse to the rest of the lists
   [else (lex<=? (rest x) (rest y))]))

(module+ test
  (test-case "Comparisons"
    (check-true  (lex<=? 'a       'b))
    (check-true  (lex<=? 'a       'a))
    (check-true  (lex<=? 'a       '()))
    (check-true  (lex<=? '()      '()))
    (check-true  (lex<=? '()      '(a)))
    (check-true  (lex<=? '(a)     '(b)))
    (check-true  (lex<=? '(a)     '(a)))
    (check-true  (lex<=? '(a b c) '(a c b)))

    (check-false (lex<=? 'b       'a))
    (check-false (lex<=? '()      'a))
    (check-false (lex<=? '(a)     '()))
    (check-false (lex<=? '(b)     '(a)))
    (check-false (lex<=? '(a c b) '(a b c)))))

;; Equality which allows symbols and strings
(define (ss-eq? x y)
  (cond ([symbol? x] (ss-eq? (symbol->string x)                y))
        ([symbol? y] (ss-eq?                 x (symbol->string y)))
        (#t          (equal?                 x                 y))))

(module+ test
  (test-case "ss-eq? works"
    (check-true  (ss-eq? 'foo  'foo))
    (check-true  (ss-eq? 'foo  "foo"))
    (check-true  (ss-eq? "foo" 'foo))
    (check-true  (ss-eq? "foo" "foo"))

    (check-false (ss-eq? 'foo  'bar))
    (check-false (ss-eq? 'foo  "bar"))
    (check-false (ss-eq? "foo" 'bar))
    (check-false (ss-eq? "foo" "bar"))))

;; Is X a permutation of Y?
(define (set-equal? x y)
  (equal? (list->set x) (list->set y)))
