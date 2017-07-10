#lang racket

;; Sets of replacements to make, due to definitions being alpha-equivalent.

(require "compare.rkt")
(require "lists.rkt")
(require "sets.rkt")
(require "util.rkt")

(provide count-replacements extend-replacements finalise-replacements new-of
         old-of replace replacement replacements?)

(module+ test
  (require "testing.rkt"))

;; A replacement is a set of symbols. The smallest value in the set is the one
;; we'll use (the "new"), all the rest will be replaced (the "old").

;; Remove duplicate elements. Since we know the list is sorted, we can do this
;; in linear rather than quadratic time.
(define skip-dupes
  (letrec ([go (lambda (prev lst)
                 (match lst
                   [(list)      (list)]
                   [(cons x xs) (if (equal? x prev)
                                    (go prev xs)
                                    (cons x (go x xs)))]))])
    (match-lambda
      [(list)      (list)]
      [(cons x xs) (cons x (go x xs))])))

(define (mk-rep . syms)
  (sort syms symbol<=?))

(define new-of
  car)

(define (old-of rep)
  (define new (new-of rep))
  (define go (match-lambda
               [(list)      (list)]
               [(cons x xs) (if (equal? x new)
                                (go xs)
                                (cons x xs))]))
  (go (cdr rep)))

(define (sorted-symbols-list? x)
  (and (list? x)
       (all-of symbol? x)
       (equal? x (sort x symbol<=?))))

(define (replacement? rep)
  (and (sorted-symbols-list? rep)
       (not (equal? 0 (length rep)))))

(module+ test
  (define (compose-info f g)
    (lambda (x)
      (with-check-info
        (('x x))
        (f (g x)))))

  (def-test-case "Replacement"
    (for-each
     (compose-info check-true replacement?)
     (list (mk-rep 'A 'B)
           (mk-rep 'A 'B 'C 'D)
           (mk-rep 'foo 'bar)
           (mk-rep 'some/path.smt2A-Name 'other/file.smt2Different)))
    (for-each
     (compose-info check-false replacement?)
     (list #t
           #f
           'A
           (list)))))

(define (rep<? x y)
  (symbol<? (new-of x) (new-of y)))

(define/test-contract (disjoint? x-in y-in)
  (-> sorted-symbols-list? sorted-symbols-list? boolean?)
  (match* (x-in y-in)
    [(_           (list))      #t]
    [((list)      _)           #t]
    [((cons x xs) (cons y ys)) (cond
                                 [(equal?   x y) #f]
                                 [(symbol<? x y) (disjoint? xs   y-in)]
                                 [(symbol<? y x) (disjoint? x-in ys)])]))

(define/test-contract (overlap? x y)
  (-> sorted-symbols-list? sorted-symbols-list? boolean?)
  (not (disjoint? x y)))

(define/test-contract (merge x y)
  (-> sorted-symbols-list? sorted-symbols-list? sorted-symbols-list?)
  (match* (x y)
    [((list)      _)           y]
    [(_           (list))      x]
    [((cons a as) (cons b bs)) (if (symbol<? a b)
                                   (cons a (merge as y))
                                   (if (equal? a b)
                                       (cons b (merge as bs))
                                       (cons b (merge x  bs))))]))

(define/test-contract (insert f x xs)
  (-> (-> any/c any/c boolean?) any/c (*list/c any/c) (*list/c any/c))
  (match xs
    [(list) (list x)]
    [(cons y ys) (if (f x y)
                     (cons x xs)
                     (cons y (insert f x ys)))]))

(define insert-rep (curry insert rep<?))

;; Sorted lists let us bail out early
(define/test-contract (in? lst x)
  (-> sorted-symbols-list? symbol? boolean?)
  (match lst
    [(list) #f]
    [(cons y ys) (if (equal? x y)
                     #t
                     (if (symbol<? x y)
                         #f
                         (in? x ys)))]))

;; A set of replacements, satisfying a few consistency properties
(define (replacements? reps)
  (and (list? reps)
       (all-of replacement? reps)

       ;; No value should appear in multiple sets
       (let ([ok    #t]
             [found '()])
         (for/list ([rep reps])
           (when ok
             (set! ok (and ok (disjoint? rep found)))
             (set! found (merge found rep))))
         ok)))

(define (mk-reps . xs)
  (sort xs rep<=?))

(module+ test
  (def-test-case "Replacements"
    (for-each
     (compose-info check-true replacements?)
     (list (mk-reps)                      ;; Empty is fine
           (mk-reps (mk-rep 'B 'A))       ;; Singleton is fine
           (mk-reps (mk-rep 'B 'A) (mk-rep 'D 'C))  ;; Disjoint are fine
           (mk-reps (mk-rep 'B 'A 'C))))         ;; Multiple replacements are fine

    (for-each
     (compose-info check-false replacements?)
     (list (mk-rep 'A 'B)                        ;; Not a set of replacements
           (mk-reps (mk-rep 'B 'A) (mk-rep 'B 'C))))))  ;; Duplicate values not allowed

(define/test-contract (any->bool x)
  (-> any/c boolean?)
  (not (not x)))

(define (rep-equal? x y)
  (equal? x y))

(define (rep<=? a b)
  (symbol<=? (new-of a) (new-of b)))

(define (reps-equal? x y)
  (equal? (sort x rep<=?) (sort y rep<=?)))

(define (merge-overlaps modified? acc reps)
  (match reps
    ;; Keep passing over the reps until no more modifications take place
    [(list) (if modified?
                (merge-overlaps #f '() acc)
                acc)]
    ;; Merge r into any overlaps in rs
    [(cons r rs) (match (do-merge '() #f r rs)
                   [(list rep new-rs mod?)

                    (merge-overlaps (or modified? mod?)
                                    (cons rep acc)
                                    new-rs)])]))

;; Merges rep into anything in reps that overlaps
(define (do-merge acc mod? rep reps)
  (match reps
    [(list)      (list rep acc mod?)]
    [(cons r rs) (if (disjoint? r rep)
                     (do-merge (cons r acc) mod?          rep  rs)
                     (do-merge         acc #t    (merge r rep) rs))]))

(define (reps-insert-rep-acc acc-merge acc-reps rep reps)
  (match reps
    [(list)      (cons (foldl merge rep acc-merge) acc-reps)]
    [(cons r rs) (if (disjoint? r rep)
                     ;; Accumulate r and recurse over the tail
                     (reps-insert-rep-acc acc-merge (cons r acc-reps) rep rs)
                     ;; Mark r for merging and continue
                     (reps-insert-rep-acc (cons r acc-merge) acc-reps rep rs))]))

(define (reps-insert-rep rep reps)
  (reps-insert-rep-acc '() (mk-reps) rep reps))

(define reps-union (curry foldl reps-insert-rep))

;; Takes in a possibly-malformed set of replacements: symbols are allowed to
;; appear in multiple sets. We merge such overlapping sets to output a valid
;; set of replacements.
(define merge-replacements (curry reps-union (mk-reps)))

(define/test-contract (extend-replacements . repss)
  (-> replacements? ... replacements?)

  (define/match (postprocess x)
    [((list))          (list)]
    [((cons rep reps)) (let ([rep2 (skip-dupes rep)])
                          (if (equal? 1 (length rep2))
                              (postprocess reps)
                              (cons rep2 (postprocess reps))))])

  ;; Combine all sets, then sort out internal consistency
  ;(postprocess (merge-replacements (foldl reps-union (mk-reps) repss)))
  (define (combine acc repss)
    (match repss
      [(list) acc]
      [(cons (cons rep reps) rest) (let ([rep2 (skip-dupes rep)])
                                     (combine (if (< 2 (length rep2))
                                                  acc
                                                  (cons rep2 acc))
                                              (cons reps rest)))]))
  (postprocess (merge-overlaps #f '() (append* repss)))
  )

(module+ test
  (def-test-case "Extend one set of replacements with another"
    (check-equal? (finalise-replacements
                   (mk-reps (mk-rep 'A 'B 'C) (mk-rep 'D 'E 'F)))
                  (finalise-replacements
                   (foldl extend-replacements
                          (replacement)
                          (list (replacement 'A 'B)
                                (replacement 'A 'C)
                                (replacement 'D 'E 'F)
                                (replacement 'G)))))))

(define final-replacements? (hash/c symbol? symbol?))

;; Convert a set of replacements into an old => new mapping, which is more
;; efficient for lookups.
(define/test-contract (finalise-replacements reps)
  (-> replacements? final-replacements?)
  (for/fold ([result (hash)])
            ([rep    reps])
    (define new (new-of rep))

    (for/fold ([output (hash-set result new new)])
              ([old    (old-of rep)])
      (hash-set output old new))))

(define/test-contract (replacement . xs)
  (-> symbol? ... replacements?)
  (if (empty? xs)
      (mk-reps)
      (mk-reps (apply mk-rep xs))))

(module+ test
  (def-test-case "Finalise replacements"
    (check-equal? (hash)
                  (finalise-replacements (mk-reps))
                  "Empty")

    (check-equal? (hash 'A 'A 'B 'A)
                  (finalise-replacements (mk-reps (mk-rep 'A 'B)))
                  "Single replacement")

    (check-equal? (hash 'A 'A 'B 'A 'C 'A)
                  (finalise-replacements (mk-reps (mk-rep 'C 'A 'B)))
                  "Replace multiple names with one")

    (check-equal? (hash 'A 'A
                        'B 'A
                        'C 'C
                        'D 'C)
                  (finalise-replacements (mk-reps (mk-rep 'A 'B)
                                                  (mk-rep 'D 'C)))
                  "Multiple disjoint sets")

    (check-equal? (hash 'A 'A
                        'B 'A
                        'C 'A
                        'D 'D
                        'E 'D)
                  (finalise-replacements (mk-reps (mk-rep 'A 'B 'C)
                                                  (mk-rep 'E 'D)))
                  "Multiple entries")))

(define/test-contract (replace f-reps x)
  (-> final-replacements? any/c any/c)
  (hash-foldl replace-in x f-reps))

(define count-replacements length)
