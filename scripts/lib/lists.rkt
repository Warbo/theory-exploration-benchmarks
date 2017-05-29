#lang racket

;; Useful list functions

(module+ test
  (require rackunit))

;; Replace all occurrences of OLD with REPLACEMENT in EXPR
(define (replace-in old replacement expr)
  (if (equal? old expr)
      replacement
      (match expr
        [(cons a b) (cons (replace-in old replacement a)
                          (replace-in old replacement b))]
        [_          expr])))

;; For each (OLD NEW) in REPS, replace OLD with NEW in EXPR
(define (replace-all reps expr)
  (foldl (lambda (rep expr)
           (replace-in (first rep) (second rep) expr))
         expr
         reps))

;; Apply F to each element of XS, and append the results together
(define (concat-map f xs)
  (append* (map f xs)))

(module+ test
  (test-case "Can concat-map"
    (check-equal? (concat-map (lambda (x) (list x x x))
                              '(fee fi fo fum))
                  '(fee fee fee fi fi fi fo fo fo fum fum fum))))

;; Backported from Racket 6.7
(define index-where
  (let ()
    (define (index-where-acc acc lst proc)
      (if (empty? lst)
          #f
          (if (proc (car lst))
              acc
              (index-where-acc (+ 1 acc) (cdr lst) proc))))
    (lambda (lst proc)
      (index-where-acc 0 lst proc))))

;; Returns the last N elements of LST
(define (take-from-end n lst)
  (reverse (take (reverse lst) n)))

(define (non-empty? x)
  (not (empty? x)))

;; Creates a list of pairs '((X1 Y1) (X2 Y2) ...) when given a pair of lists
;; '(X1 X2 ...) and '(Y1 Y2 ...)
(define (zip xs ys)
  (if (empty? xs)
      null
      (if (empty? ys)
          null
          (cons (list (car xs) (car ys))
                (zip  (cdr xs) (cdr ys))))))

;; Returns TRUE if any element of XS passes predicate F, FALSE otherwise
(define (any-of f xs)
  (foldl (lambda (x y)
           (or (f x) y))
         #f
         xs))

;; Returns FALSE if any element of XS fails predicate F, TRUE otherwise
(define (all-of f xs)
  (foldl (lambda (x y)
           (and (f x) y))
         #t
         xs))

(define ((assoc-contains? . keys) l)
  (unless (list? l)
    (raise-user-error
     'assoc-contains
     "Expected a list, given ~s" l))
  (all-of (lambda (key)
            (or (any-of (lambda (pair)
                          (and (pair? pair)
                               (equal? (car pair) key)))
                        l)
                (raise-user-error
                 'assoc-contains
                 "Couldn't find entry for ~s in ~s" key l)))
          keys))

(define (assoc-get key val)
  (second (assoc key val)))

;; Deterministically, but unpredictably, shuffle the given NAMES. KEYGEN turns
;; a name into a hash, and we perform the shuffle by sorting the hashes.
(define (deterministic-shuffle keygen names)
  (define sorted
    (sort (map (lambda (n) (list n (keygen n)))
               names)
          (lambda (x y)
            (not (bytes>? (second x) (second y))))))
  (map first sorted))
