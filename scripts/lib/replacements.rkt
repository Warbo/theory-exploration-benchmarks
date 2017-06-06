#lang racket

;; Sets of replacements to make, due to definitions being alpha-equivalent.

(require data/heap)
(require racket/trace)
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

(define (mk-rep . syms)
  (vector->heap symbol<=? (apply vector syms)))

(define new-of
  heap-min
  #;(set-foldl (lambda (x min)
               (cond
                 [(equal? min #f)  x]
                 [(symbol<? x min) x]
                 [else min]))
             #f
             s))

(define (pop-until-distinct new s)
  (if (or (equal? (heap-count s) 0)
          (not (equal? (new-of s) new)))
      s
      (pop-until-distinct new (heap-pop s))))

(define (heap-pop h)
  (define result (heap-copy h))
  (heap-remove-min! result)
  result)

(define (old-of s)
  ;; We allow a replacement? heap to contain duplicate values, since it keeps
  ;; the implementation simple and doesn't affect the eventual replace. However,
  ;; we do require that old-of and new-of have distinct outputs, so we use a
  ;; loop here to remove all copies of new-of, to account for possible dupes.
  (pop-until-distinct (new-of s) (heap-copy s))
  #;(set-remove s (new-of s)))

(define (render-rep rep)
  (map heap->vector (vector->list (heap->vector rep))))

(define (replacement? rep)
  (and (heap? rep)
       (not (equal? 0 (heap-count rep)))
       (all-of symbol? (vector->list (heap->vector rep))))
  #;(and (set?   rep)
       (not (set-empty? rep))
       (all-of symbol? (set->list rep))))

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
           (mk-rep 'some/path.smt2A-Name 'other/file.smt2Different))
     #;
     (list (set 'A 'B)
           (set 'A 'B 'C 'D)
           (set 'foo 'bar)
           (set 'some/path.smt2A-Name 'other/file.smt2Different)))
    (for-each
     (compose-info check-false replacement?)
     (list #t
           #f
           'A
           (make-heap symbol<=?))
     #;
     (list #t
           #f
           'A
           (set)
           (set "A" 'B)
           (set 'A "B")))))

(define/test-contract (disjoint? x y)
  (-> replacement? replacement? boolean?)
  (set-empty? (set-intersect x y)))

(define/test-contract (overlap? x y)
  (-> replacement? replacement? boolean?)
  (not (disjoint? x y)))

;; A set of replacements, satisfying a few consistency properties
(define (replacements? reps)
  (and (heap? reps)
       (all-of replacement? (vector->list (heap->vector reps)))

       ;; No value should appear in multiple sets
       (let ([ok    #t]
             [found '()])
         (for/list ([rep (in-heap reps)])
           (when ok
             (when (any-of (lambda (x) (member x found))
                           (vector->list (heap->vector rep)))
               (set! ok #f)))
           (set! found (append found (vector->list (heap->vector rep)))))
         ok)
       #;
       (first (set-foldl (lambda (rep result)
                           (match result
                             [(list ok found)
                              ;; Short-circuit if a problem was already found
                              (if ok
                                  ;; Check if anything in rep was already found
                                  (list (or (set-empty? found)
                                            (disjoint? found rep))
                                        ;; Extend found with the elements of rep
                                        (set-union found rep))
                                  ;; Pass along existing problem
                                  result)]))
                         (list #t (set)) ;; Starts OK with nothing found yet
                         reps)))
  #;
  (and (set? reps)
       (all-of replacement? (set->list reps))

       ;; No value should appear in multiple sets
       (first (set-foldl (lambda (rep result)
                           (match result
                             [(list ok found)
                              ;; Short-circuit if a problem was already found
                              (if ok
                                  ;; Check if anything in rep was already found
                                  (list (or (set-empty? found)
                                            (disjoint? found rep))
                                        ;; Extend found with the elements of rep
                                        (set-union found rep))
                                  ;; Pass along existing problem
                                  result)]))
                         (list #t (set)) ;; Starts OK with nothing found yet
                         reps))))

;; Comparison for rep sets. Not strictly needed, but lets us use heaps or heaps.
(define (rep<=? x y)
  (symbol<=? (new-of x) (new-of y)))

(define (mk-reps . xs)
  (vector->heap rep<=? (list->vector xs)))

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

;; Merge the given rep into an overlapping member of reps (returns 'merged and
;; the new set) or do nothing if none overlap (returns 'unmerged and the
;; existing set). NOTE: Only merges into one member; the result may be an
;; invalid set of replacements, e.g. if rep overlaps two members of reps.
(define/test-contract (merge-if-overlap rep reps)
  (-> replacement? replacements? (list/c symbol? replacements?))

  (define found    #f)
  (define result   (heap-copy reps))
  (define rep-list (vector->list (heap->vector rep)))

  (for/list ([this-rep (in-heap reps)])
    (define this-list
      (vector->list (heap->vector this-rep)))

    (unless found
      (when (any-of (lambda (elem)
                      (member elem this-list))
                    rep-list)
        (set! found #t)

        ;; Combine this-rep and rep together

        (define new (mk-rep))
        (heap-add-all! new this-rep)
        (heap-add-all! new rep)

        ;; Replace this-rep in result with our new merged value

        (heap-remove! result this-rep)
        (heap-add!    result new))))
  (list (if found 'merged 'unmerged) result)
  #;
  (set-foldl (lambda (existing-rep result)
               (if (or (equal? (first result) 'merged) ;; Already merged
                       (disjoint? existing-rep rep))   ;; Unmergeable
                   ;; Add as-is
                   (list (first result) (set-add (second result) existing-rep))

                   ;; Merge in
                   (list 'merged (set-add (second result)
                                          (set-union existing-rep rep)))))
             (list 'unmerged (set))
             reps))

(define (rep-equal? x y)
  (equal? (heap->vector x) (heap->vector y)))

(define (reps-equal? x y)
  (define different #f)
  (for ([x2 (in-heap x)]
        [y2 (in-heap y)])
    (unless (rep-equal? x2 y2)
      (set! different #t)))
  (not different))

(module+ test
  (def-test-case "Merge overlapping replacements"
    (define empty (merge-if-overlap (mk-rep 'A 'B) (mk-reps)))
    (check-equal? (first empty) 'unmerged
                  "Empty unmerged")

    (check-true (reps-equal? (second empty) (mk-reps))
                "Empty still empty")

    (check-true (reps-equal? (mk-reps (mk-rep 'C 'D) (mk-rep 'E 'F))
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

(define (heap-union x y)
  (define result (heap-copy x))
  (heap-add-all! result y)
  result)

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
(define/test-contract (merge-replacements reps)
  (-> (and/c heap?
             (lambda (reps)
               (all-of (lambda (rep)
                         (and (heap? rep)
                              (all-of symbol?
                                      (vector->list (heap->vector rep)))))
                       (vector->list (heap->vector reps)))))
      replacements?)
  (match (partial-merge reps)
    [(list 'merged   reps) (merge-replacements reps)]
    [(list 'unmerged reps) reps]))

(define/test-contract (extend-replacements . repss)
  (-> replacements? ... replacements?)

  ;; Combine all sets, then sort out internal consistency
  (merge-replacements (foldl heap-union (mk-reps) repss)))

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
            ([rep    (in-heap reps)])
    (define new (new-of rep))

    (for/fold ([output (hash-set result new new)])
              ([old    (in-heap (old-of rep))])
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

(define (count-replacements x)
  (heap-count x))
