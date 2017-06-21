#lang racket

;; Sets of replacements to make, due to definitions being alpha-equivalent.

(require data/heap)
(require racket/trace)
(require (only-in srfi/43 vector-binary-search))
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

(define (render-rep rep)
  (map heap->vector (vector->list (heap->vector rep))))

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
                                   (cons b (merge x  bs)))]))

(define/test-contract (insert f x xs)
  (-> any/c (*list/c any/c) (*list/c any/c))
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

(define/test-contract (vec-contains? v x)
  (-> (vectorof symbol?)
      symbol?
      boolean?)
  (any->bool (vector-binary-search v x (lambda (a b)
                                         (if (symbol<? a b)
                                             -1
                                             (if (symbol<? b a)
                                                 1
                                                 0))))))

;; Merge the given rep into an overlapping member of reps (returns 'merged and
;; the new set) or do nothing if none overlap (returns 'unmerged and the
;; existing set). NOTE: Only merges into one member; the result may be an
;; invalid set of replacements, e.g. if rep overlaps two members of reps.
(define/test-contract (merge-if-overlap rep reps)
  (-> replacement? replacements? (list/c symbol? replacements?))

  (match/values (for/fold ([found  #f]
                           [result (list)])
                          ([this-rep reps])
                  (if found
                      (values found (cons this-rep result))
                      (let ([overlapping (overlap? this-rep rep)])
                        (values overlapping
                                (cons (if overlapping
                                          (merge this-rep rep)
                                          this-rep)
                                      result)))))
    [(found result) (list (if found 'merged 'unmerged) result)]))

(define (rep-equal? x y)
  (equal? x y))

(define (rep<=? a b)
  (symbol<=? (new-of a) (new-of b)))

(define (reps-equal? x y)
  (equal? (sort x rep<=?) (sort y rep<=?)))

(module+ test
  (def-test-case "Merge overlapping replacements"
    (define empty (merge-if-overlap (mk-rep 'A 'B) (mk-reps)))
    (check-equal? (first empty) 'unmerged
                  "Empty unmerged")

    (check-equal? (second empty) (mk-reps)
                "Empty still empty")

    (check-equal? (reps-equal?
                   (mk-reps (mk-rep 'C 'D) (mk-rep 'E 'F))
                   (second (merge-if-overlap
                            (mk-rep 'A 'B) (mk-reps (mk-rep 'C 'D)
                                                    (mk-rep 'E 'F)))))
                "Disjoint don't merge")

    (let ()
      (define expected
        (mk-reps (mk-rep 'C 'D) (mk-rep 'A 'B 'E)))

      (define actual
        (merge-if-overlap
         (mk-rep 'A 'B) (mk-reps (mk-rep 'C 'D)
                                 (mk-rep 'E 'B))))

      (with-check-info
        (('actual   (render-rep (second actual)))
         ('expected (render-rep expected)))
        (check-equal? (finalise-replacements expected)
                      (finalise-replacements (second actual))
                      "Single overlap merges")))))

(define (heap-add h x)
  (define result (heap-copy h))
  (heap-add! result x)
  result)

(define (reps-insert-rep-acc acc rep reps)
  (match reps
    [(list)      (cons rep acc)]
    [(cons r rs) (if (disjoint? r rep)
                     ;; Accumulate r and recurse over the tail
                     (reps-insert-rep-acc (cons r acc) rep rs)
                     ;; Merge r into reps, and start again
                     (reps-insert-rep-acc (mk-reps) (merge r rep)
                                          (append rs acc)))]))

(define (reps-insert-rep rep reps)
  (sort (reps-insert-rep-acc (mk-reps) rep reps) rep<=?))

(define reps-union (curry foldl reps-insert-rep))

;; Merge overlapping replacements in the given set, returning 'merged and the
;; new set. If no overlaps were found, returns 'unmerged and the original set.
;; If 'merged is returned, there may still be more overlaps to find (hence the
;; name partial).
(define (partial-merge reps)
  (for/fold ([result (list 'unmerged (mk-reps))])
            ([rep    (in-heap reps)])
    (match (merge-if-overlap rep (second result))
      [(list 'merged   reps) (list 'merged        reps)]
      [(list 'unmerged reps) (list (first result) (heap-add reps rep))])))

(module+ test
  (def-test-case "Partial merge"
    (check-equal? (finalise-replacements  (mk-reps (mk-rep 'A 'B)
                                                   (mk-rep 'C 'D)))
                  (finalise-replacements
                   (second (partial-merge (mk-reps (mk-rep 'A 'B)
                                                   (mk-rep 'C 'D)))))
                  "Disjoint don't partially merge")))

;; Takes in a possibly-malformed set of replacements: symbols are allowed to
;; appear in multiple sets. We merge such overlapping sets to output a valid
;; set of replacements.
(define merge-replacements (curry reps-union (mk-reps)))

(define/test-contract (extend-replacements . repss)
  (-> replacements? ... replacements?)

  ;; Combine all sets, then sort out internal consistency
  (merge-replacements (foldl reps-union (mk-reps) repss)))

(module+ test
  (def-test-case "Extend one set of replacements with another"
    (check-equal? (finalise-replacements
                   (mk-reps (mk-rep 'A 'B 'C) (mk-rep 'D 'E 'F) (mk-rep 'G)))
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
