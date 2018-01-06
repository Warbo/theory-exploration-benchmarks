#lang racket

(require grommet/crypto/hash/sha256)
(require lib/compare)
(require lib/impure)
(require lib/normalise)
(require lib/theorems)
(require lib/tip)
(require lib/lists)
(require lib/util)

(provide get-sampling-data make-sampling-data sample-from-benchmarks)

(module+ test
  (require lib/testing))

;; Does S contain all dependencies required by some theorem statement?
(define (sample-admits-conjecture? s)
  (any-of (lambda (f-deps)
            (subset? (second f-deps) s))
          (all-theorem-deps)))

;; Deterministically, but unpredictably, select a sample of NAMES. The sample
;; size is given by SIZE, whilst REP provides entropy for making the choices
;; (e.g. you can run with REP as 0, 1, 2, etc. to get different results).
(define/test-contract (sample size rep names given-constraints)
  (->i ([size              integer?]
        [rep               integer?]
        [names             (*list/c symbol?)]
        [given-constraints
         (names size)
         (lambda (given-constraints)
           (unless (list? given-constraints)
             (raise-user-error
              'sample
              "Expected constraints list, got ~a" given-constraints))
           (when (empty? given-constraints)
             (raise-user-error
              'sample
              "No constraints given"))
           (for-each
            (lambda (constraint)
              (unless (set? constraint)
                (raise-user-error
                 'sample
                 "Expected constraint to be set, got ~a" constraint))
              (when (empty? (set->list constraint))
                (raise-user-error
                 'sample
                 "Empty constraint given"))
              (for-each
               (lambda (name)
                 (unless (member name names)
                   (raise-user-error
                    'sample
                    "Constraint ~a contains names not in ~a"
                    constraint
                    names)))
               (set->list constraint)))
            given-constraints)
           (when (empty? (filter (lambda (c)
                                   (<= (set-count c) size))
                                 given-constraints))
             (raise-user-error
              'sample
              "Sample size ~a too small for constraints ~a"
              size given-constraints))
           #t)])
       [result (names given-constraints size)
               (lambda (result)
                 (unless (set? result)
                   (raise-user-error
                    'sample
                    "Expected sample to be a set, produced: ~a" result))
                 (unless (equal? (set-count result) size)
                   (raise-user-error
                    'sample
                    "Expected sample to have size ~a, produced: ~a"
                    size result))
                 (for-each
                  (lambda (name)
                    (unless (member name names)
                      (raise-user-error
                       'sample
                       "Expected sampled names to be in ~a, produced: ~a"
                       names result)))
                  (set->list result))
                 (when (empty? (filter (lambda (c)
                                         (subset? c result))
                                       given-constraints))
                   (raise-user-error
                    'sample
                    "Sample ~a isn't a superset of any ~a"
                    result given-constraints)))])

  ;; We get "deterministic randomness" by using this hash function. Before
  ;; hashing, we prefix the given value with the given SIZE and REP, so
  ;; different sample parameters will produce different outputs, but the same
  ;; parameters will get the same outputs.
  (define (get-hash val)
    (sha256 (format "sample-size-~a-selection-round-~a-~a" size rep val)))

  ;; To avoid division by zero in recall experiments, we only return
  ;; samples which include the dependencies of at least one theorem (which we
  ;; call "constraints").

  ;; We could achieve this using rejection sampling: keep generating samples
  ;; until one is a superset of some constraint, and return that. However,
  ;; that becomes very inefficient when the constraints are unlikely to be found
  ;; by chance, e.g. with a large space of names, few constraints, and large
  ;; constraints.

  ;; Instead, we perform a massive optimisation: we pick a constraint to begin
  ;; with, then pad it with uniformly chosen names until we reach the sample
  ;; size. This guarantees the postcondition without any rejecting/backtracking.

  ;; First, gather our constraints, discarding duplicates. Also discard those
  ;; which are larger than the sample size, since they can never be satisfied.
  (define constraints
    (remove-duplicates (filter (lambda (c)
                                 (<= (set-count c) size))
                               given-constraints)))

  (when (empty? constraints)
    (error (format "Couldn't find deps for sample size ~a" size)))

  (msg "Converted all theorem dependencies into constraints\n")

  ;; Constraints containing few names are more likely to be chosen by uniform
  ;; sampling than those containing many names, and hence are more likely to be
  ;; generated by the rejection sampling procedure. Rather than working with
  ;; probabilities, we work with *relative frequencies*: let's say we choose M
  ;; names uniformly at random, how likely are we to find one constraint
  ;; compared to another?

  ;; Since the names are chosen uniformly, we can just divide up M by the length
  ;; of each constraint. For example, if a constraint has length 2 then the
  ;; first two names in M might match, or the next two, or the next two, and so
  ;; on, giving M / 2 chances for this constraint to appear. If a constraint has
  ;; length 10, there are only M / 10 chances for it to appear.

  ;; To make the sums easy, we choose M to be the *least common multiple* of the
  ;; lengths: a number which all the lengths divide into without a remainder.
  (define constraint-lcm
    (apply lcm (map set-count constraints)))

  ;; Now we calculate the relative chances of each constraint appearing in a
  ;; uniform sample, by just performing these divisions.
  (define constraint-freqs
    (map (lambda (c)
           (list c (/ constraint-lcm (set-count c))))
         constraints))

  (msg "Calculated frequency for each constraint\n")

  ;; Now we know the relative chances, we can choose a constraint, following the
  ;; same distribution as if we'd used rejection sampling.

  ;; We'll work in the interval [0, t), i.e. including 0 but excluding t, where
  ;; t is the sum of all the frequencies.
  (define freq-sum
    (apply + (map second constraint-freqs)))

  ;; We divide up this interval between the constraints, giving each a
  ;; sub-interval with size equal to that constraint's frequency. These
  ;; sub-intervals occur in lexicographic order of the constraints.
  (define intervals
    (foldl (lambda (c-freq prev)
             ;; Start at 0, or where the last interval ends
             (define start
               (if (empty? prev)
                   0
                   (third (first prev))))

             (cons (list (first c-freq)               ;; constraint
                         start                        ;; interval start
                         (+ start (second c-freq)))  ;; interval end
                   prev))
           '()
           (sort constraint-freqs
                 (lambda (c1 c2)
                   (define c1-sorted
                     (sort (set->list (first c1)) symbol<=?))

                   (define c2-sorted
                     (sort (set->list (first c2)) symbol<=?))

                   (arbitrary<=? c1-sorted c2-sorted)))))

  ;; Now we choose an arbitrary number in this interval. To remain deterministic
  ;; we use a hash rather than a random number generator. We interpret the bytes
  ;; of the hash as an integer written in base 256, then use modulo to get the
  ;; desired range.
  (define chosen-num
    (modulo (foldl (lambda (b rest) (+ b (* 256 rest)))
                   0
                   (bytes->list (get-hash "chosen-num")))
            freq-sum))

  ;; We choose the constraint whose sub-interval contains our chosen number
  (define chosen-constraint
    (match (filter (lambda (i)
                     (match i
                       [(list _ start end) (and (<= start      chosen-num)
                                                (<  chosen-num end))]))
                   intervals)
      [(list (list c _ _)) c]))

  (msg "Shuffling names\n")

  ;; Now we need to pad our constraint with uniformly chosen names. To do this,
  ;; we shuffle all of the names and choose from the start of the list.
  (define shuffled
    (deterministic-shuffle get-hash names))

  (define padding
    (take (remove* (set->list chosen-constraint) shuffled)
          (- size (set-count chosen-constraint))))

  (msg "Obtained sample\n")

  (list->set (append (set->list chosen-constraint) padding)))

(module+ test
    (def-test-case "Sampling"

    (define names
      '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

    (define name-constraints
      (map (lambda (x) (list->set (list x)))
           names))

    ;; There's no deep reason for these values, they're just the results spat
    ;; out when this test was added. Since sampling is deterministic, these
    ;; shouldn't change, unless e.g. we change the hashing function.

    (check-equal? (sample 5 0 names name-constraints)
                  (list->set '(a j q t w)))

    (check-equal? (sample 5 1 names name-constraints)
                  (list->set '(i t u x y)))

    ;; Check invariants over a selection of values
    (for-each (lambda (size)
                (for-each (lambda (rep)
                            (define s (sample size rep names name-constraints))

                            (check-true (subset? s (list->set names)))

                            (check-equal? size (set-count s)))
                          (range 0 10)))
              (range 1 10))))

;; Cache data required for sampling in /tmp, so we can draw samples over and
;; over from the same benchmarks without recalculating everything each time.
(define sampling-data?
  (assoc-contains? 'all-canonical-function-names
                   'theorem-deps))

(define/test-contract (make-sampling-data)
  (-> sampling-data?)
  `((all-canonical-function-names
     ;; Theorem deps aren't hex encoded, so sample with
     ;; decoded versions
     ,(map decode-name (lowercase-benchmark-names)))

    ;; read/write doesn't work for sets :(
    (theorem-deps
     ,(map (lambda (t-d)
             (list (first t-d) (set->list (second t-d))))
           (all-theorem-deps)))

    (normalised-theorems
     ,(normalised-theorems))))

(define/test-contract (get-sampling-data)
  (-> sampling-data?)
  (read-from-cache! "BENCHMARKS_CACHE" make-sampling-data))

;; Sample using the names and theorems from BENCHMARKS
(define (sample-from-benchmarks size rep)
  (define data (get-sampling-data))

  (define-values (all-canonical-function-names theorem-deps)
    (values (assoc-get 'all-canonical-function-names data)
            (map (lambda (t-d)
                   (list->set (second t-d)))
                 (assoc-get 'theorem-deps data))))

  (define all-constructors
    (strip-matching-prefix all-canonical-function-names "constructor-"))

  (define all-destructors
    (strip-matching-prefix all-canonical-function-names "destructor-"))

  ;; TODO: Rather than filtering out constructors and destructors, we shouldn't
  ;; be including them in the first place. In particular, the naming is pretty
  ;; deceptive.

  (define only-function-names
    (remove* (append all-constructors all-destructors)
             all-canonical-function-names))

  (define sampled
    (sample size rep only-function-names theorem-deps))

  ;; Hex encode sample so it's usable with e.g. Haskell translation
  (map-set encode-lower-name sampled))

(define (strip-matching-prefix names p)
  ;; Remove the prefix 'p' from the start of each result
  (map (lambda (name)
         (string->symbol
          (substring (symbol->string name)
                     (string-length p))))

       ;; The results are those names beginning with 'p'
       (filter (lambda (name)
                 (string-prefix? (symbol->string name) p))
               names)))

(module+ test
  (def-test-case "Smart sampling"
    (define all-deps
      (apply set-union (map second (all-theorem-deps))))

    (define sampled
      (map-set decode-name (sample-from-benchmarks 10 0)))

    (with-check-info
      (('sampled sampled))
      (check-true (sample-admits-conjecture? sampled)
                  "Sampling from benchmark files should admit their theorems"))

    (for-each (lambda (f-deps)
                (define deps (second f-deps))

                (check-true (sample-admits-conjecture? deps)
                            "A theorem's deps admit at least one theorem")

                (check-equal? deps
                              (sample (set-count deps)
                                      0
                                      (set->list deps)
                                      (list deps))
                              "Sampling just from deps returns those deps")

                (check-equal? deps
                              (sample (set-count deps)
                                      0
                                      (append (set->list deps)
                                              '(a b c d e f g h i j k l m
                                                n o p q r s t u v w x y z))
                                      (list deps))
                              "Sampling with one deps constraint returns deps"))
              (quick-or-full (take (shuffle (all-theorem-deps)) 10)
                             (all-theorem-deps))))

  (def-test-case "Only functions get sampled"
    (define all-functions
      (map decode-name (append-map toplevel-function-names-in
                                   (final-benchmark-defs))))

    (define all-constructors
      (strip-matching-prefix all-functions "constructor-"))

    (define all-destructors
      (strip-matching-prefix all-functions "destructor-"))

    (for-each (lambda (rep)
                (for-each (lambda (encoded)
                            (define name (decode-name encoded))

                            (with-check-info
                              (('encoded           encoded)
                               ('name              name)
                               ('some-constructors (take all-constructors 5)))
                              (check-false
                               (member name all-constructors)
                               "Shouldn't sample constructors"))

                            (with-check-info
                              (('encoded           encoded)
                               ('name              name)
                               ('some-destructors (take all-destructors 5)))
                              (check-false
                               (member name all-destructors)
                               "Shouldn't sample destructors"))

                            (with-check-info
                              (('encoded        encoded)
                               ('name           name)
                               ('some-functions (take all-functions 5)))
                              (check-true
                               (any->bool (member name all-functions))
                               "Sample should be function names")))

                          ;; We want a whole bunch of names, so we pick 50
                          (set->list
                           (sample-from-benchmarks (quick-or-full 5 50) rep))))
              (range 0 (quick-or-full 5 100)))))
