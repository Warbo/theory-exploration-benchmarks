#lang racket

;; Sets of replacements to make, due to definitions being alpha-equivalent.

(require racket/trace)
(require "lists.rkt")
(require "sets.rkt")
(require "util.rkt")

(provide extend-replacements finalise-replacements new-of old-of replace
         replacement replacements?)

(module+ test
  (require "testing.rkt"))

;; A replacement is a set of symbols. The smallest value in the set is the one
;; we'll use (the "new"), all the rest will be replaced (the "old").

(define (new-of s)
  (set-foldl (lambda (x min)
               (cond
                 [(equal? min #f)  x]
                 [(symbol<? x min) x]
                 [else min]))
             #f
             s))

(define (old-of s)
  (set-remove s (new-of s)))

(define (replacement? rep)
  (and (set?   rep)
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
     (list (set 'A 'B)
           (set 'A 'B 'C 'D)
           (set 'foo 'bar)
           (set 'some/path.smt2A-Name 'other/file.smt2Different)))
    (for-each
     (compose-info check-false replacement?)
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
                         (list #t (set))  ;; Starts OK with nothing found yet
                         reps))))

(module+ test
  (def-test-case "Replacements"
    (for-each
     (compose-info check-true replacements?)
     (list (set)                          ;; Empty is fine
           (set (set 'B 'A))              ;; Singleton is fine
           (set (set 'B 'A) (set 'D 'C))  ;; Disjoint are fine
           (set (set 'B 'A 'C))))         ;; Multiple replacements are fine

    (for-each
     (compose-info check-false replacements?)
     (list (set 'A 'B)                        ;; Not a set of replacements
           (set (set 'B 'A) (set 'B 'C))))))  ;; Duplicate values not allowed

;; Merge the given rep into an overlapping member of reps (returns 'merged and
;; the new set) or do nothing if none overlap (returns 'unmerged and the
;; existing set). NOTE: Only merges into one member; the result may be an
;; invalid set of replacements, e.g. if rep overlaps two members of reps.
(define (merge-if-overlap rep reps)
  (set-foldl (lambda (existing-rep result)
               (if (or (equal? (first result) 'merged)  ;; Already merged
                       (disjoint? existing-rep rep))    ;; Unmergeable
                   ;; Add as-is
                   (list (first result) (set-add (second result) existing-rep))

                   ;; Merge in
                   (list 'merged (set-add (second result)
                                          (set-union existing-rep rep)))))
             (list 'unmerged (set))
             reps))

(module+ test
  (def-test-case "Merge overlapping replacements"
    (check-equal? (merge-if-overlap (set 'A 'B) (set))
                  (list 'unmerged (set))
                  "Empty never overlaps")

    (check-equal? (merge-if-overlap (set 'A 'B) (set (set 'C 'D) (set 'E 'F)))
                  (list 'unmerged (set (set 'C 'D) (set 'E 'F)))
                  "Disjoint don't merge")

    (check-equal? (merge-if-overlap (set 'A 'B) (set (set 'C 'D) (set 'E 'B)))
                  (list 'merged (set (set 'C 'D) (set 'A 'B 'E)))
                  "Single overlap merges")

    ;; Sets are unordered, so multiple overlaps are non-deterministic
    (let ()
      (define result
        (merge-if-overlap (set 'A 'B) (set (set 'B 'C) (set 'A 'D))))
      (with-check-info
        (('result result))
        (check-true (or (equal? result (list 'merged
                                             (set (set 'A 'B 'C) (set 'A 'D))))
                        (equal? result (list 'merged
                                             (set (set 'B 'C) (set 'A 'B 'D)))))
                    "Multiple overlaps merge once")))))

;; Merge overlapping replacements in the given set, returning 'merged and the
;; new set. If no overlaps were found, returns 'unmerged and the original set.
;; If 'merged is returned, there may still be more overlaps to find (hence the
;; name partial).
(define (partial-merge reps)
  (set-foldl (lambda (rep result)
               (match (merge-if-overlap rep (second result))
                 [(list 'merged   reps) (list 'merged reps)]
                 [(list 'unmerged reps) (list (first result)
                                              (set-add reps rep))]))
             (list 'unmerged (set))
             reps))

(module+ test
  (def-test-case "Partial merge"
    (check-equal? (partial-merge  (set (set 'A 'B) (set 'C 'D)))
                  (list 'unmerged (set (set 'A 'B) (set 'C 'D)))
                  "Disjoint don't merge")))

;; Takes in a possibly-malformed set of replacements: symbols are allowed to
;; appear in multiple sets. We merge such overlapping sets to output a valid
;; set of replacements.
(define/test-contract (merge-replacements reps)
  (-> (and/c set? (lambda (reps) (all-of (lambda (rep)
                                           (and (set? rep)
                                                (all-of symbol?
                                                        (set->list rep))))
                                         (set->list reps))))
      replacements?)
  (match (partial-merge reps)
    [(list 'merged   reps) (merge-replacements reps)]
    [(list 'unmerged reps) reps]))

;; Add rep to reps, then combine together any overlapping sets
(define/test-contract (insert-replacement rep reps)
  (-> replacement? replacements? replacements?)
  (merge-replacements (set-add reps rep)))

(module+ test
  (def-test-case "Insert replacement"
    ;; Nothing to check when empty, so inserted as-is
    (check-equal? (insert-replacement (set 'A 'B) (set))
                  (set (set 'A 'B)))

    ;; Disjoint replacements don't interfere
    (check-equal? (insert-replacement (set 'E 'F) (set (set 'A 'B) (set 'C 'D)))
                  (set (set 'E 'F) (set 'A 'B) (set 'C 'D)))

    ;; Overlapping replacements are merged
    (check-equal? (insert-replacement (set 'B 'F)
                                      (set (set 'A 'B 'C) (set 'D 'E)))
                  (set (set 'A 'B 'C 'F) (set 'D 'E)))

    ;; Overlapping multiple replacements merges all of them
    (check-equal? (insert-replacement (set 'A 'D 'G)
                                      (set (set 'A 'B) (set 'C 'D) (set 'E 'F)))
                  (set (set 'A 'B 'C 'D 'G) (set 'E 'F)))))

(define/test-contract (extend-replacements . repss)
  (-> replacements? ... replacements?)

  ;; Combine all sets, then sort out internal consistency
  (merge-replacements (apply set-union repss)))

(module+ test
  (def-test-case "Extend one set of replacements with another"
    (check-equal? (set (set 'A 'B 'C) (set 'D 'E 'F) (set 'G))
                  (foldl extend-replacements
                         (replacement)
                         (list (replacement 'A 'B)
                               (replacement 'A 'C)
                               (replacement 'D 'E 'F)
                               (replacement 'G))))))

(define final-replacements? (hash/c symbol? symbol?))

;; Convert a set of replacements into an old => new mapping, which is more
;; efficient for lookups.
(define/test-contract finalise-replacements
  (-> replacements? final-replacements?)
  (curry set-foldl
         (lambda (rep result)
           (define new (new-of rep))
           (set-foldl (lambda (name result)
                        (hash-set result name new))
                      result
                      rep))
         (hash)))

(define/test-contract (replacement . xs)
  (-> symbol? ... replacements?)
  (if (not (empty? xs))
      (set (list->set xs))
      (set)))

(module+ test
  (def-test-case "Finalise replacements"
    (check-equal? (hash)
                  (finalise-replacements (set))
                  "Empty")

    (check-equal? (hash 'A 'A 'B 'A)
                  (finalise-replacements (set (set 'A 'B)))
                  "Single replacement")

    (check-equal? (hash 'A 'A 'B 'A 'C 'A)
                  (finalise-replacements (set (set 'C 'A 'B)))
                  "Replace multiple names with one")

    (check-equal? (hash 'A 'A
                        'B 'A
                        'C 'C
                        'D 'C)
                  (finalise-replacements (set (set 'A 'B) (set 'D 'C)))
                  "Multiple disjoint sets")

    (check-equal? (hash 'A 'A
                        'B 'A
                        'C 'A
                        'D 'D
                        'E 'D)
                  (finalise-replacements (set (set 'A 'B 'C) (set 'E 'D)))
                  "Multiple entries")))

(define/test-contract (replace f-reps x)
  (-> final-replacements? any/c any/c)
  (hash-foldl replace-in x f-reps))
