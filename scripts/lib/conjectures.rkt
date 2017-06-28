#lang racket

(require json)
(require "compare.rkt")
(require "lists.rkt")
(require "normalise.rkt")
(require "sampling.rkt")
(require "theorems.rkt")
(require "tip.rkt")
(require "util.rkt")

(provide conjectures-admitted-by-sample-wrapper
         conjectures-for-sample-wrapper eqs-to-json-wrapper
         precision-recall-eqs-wrapper)

(module+ test
  (require "testing.rkt"))

(define (find-eqs-intersection found sample)
  (find-eqs-intersection-raw
   found
   (theorem-files-and-conjectures-for-sample sample)))

(define (find-eqs-intersection-raw found ground-truth)
  (map (lambda (x)
         (define eq
           (theorem-to-equation (second x)))

         (make-immutable-hash
          `((file     . ,(first  x))
            (theorem  . ,(second x))
            (equation . ,eq)
            (found    . ,(any-of (lambda (a-found)
                                   (if (empty? eq)
                                       #f
                                       (equations-match? a-found (first eq))))
                                 found)))))
       ground-truth))

(define (conjectures-from-sample found sample)
  (conjectures-from-raw found (find-eqs-intersection found sample)))

(define (conjectures-from-raw found marked)
  (define intersection
    (filter (lambda (x) (hash-ref x 'found)) marked))

  (make-immutable-hash
   `((wanted    . ,marked)
     (precision . ,(if (empty? found)
                       null
                       (real->double-flonum  (/ (length intersection)
                                                (length found)))))
     (recall    . ,(if (empty? marked)
                       null
                       (real->double-flonum (/ (length intersection)
                                               (length marked))))))))

;; Return all theorems (expressions) which would be possible to discover given
;; what's in the provided sample. In other words, those theorems whose
;; dependencies are a subset of the sample.
(define (conjectures-admitted-by sample)
  (map second (theorem-files-and-conjectures-for-sample sample)))

(module+ test
  (def-test-case "Conjecture-finding"
    (for-each (lambda (f)
                (with-check-info
                  (('file f))
                  (define conjectures
                    (conjectures-admitted-by (theorem-deps-of f)))
                  (check-not-equal? #f (member (normed-theorem-of f)
                                               conjectures)
                                    "Theorems can be derived from their deps")

                  (define super
                    (conjectures-admitted-by (append '(fee fi fo fum)
                                                     (theorem-deps-of f))))
                  (check-not-equal? #f (member (normed-theorem-of f) super)
                                    "Supersets of deps admit derivation")

                  (define eqs
                    (equations-admitted-by-sample (theorem-deps-of f)))
                  (check-true (subset? eqs conjectures)
                              "Equations are a subset of theorems")

                  (for-each (lambda (c)
                              (unless (empty? (theorem-to-equation c))
                                (check-not-equal? #f (member c eqs)
                                                  "All equations are found")))
                            conjectures)))
                (theorem-ids))))

(define (conjectures-admitted-by-sample-wrapper)
  (define sample (read-benchmark (port->string)))
  (show (conjectures-admitted-by sample)))

(define (theorem-files-and-conjectures-for-sample sample)
  (define files
    (theorem-files-admitted-by sample))

  (map (lambda (f)
         (list f (normed-theorem-of f)))
       files))

(define (theorem-files-admitted-by sample)
  (define theorem-deps
    (assoc-get 'theorem-deps (get-sampling-data)))

  (map first
       (filter (lambda (t-d)
                 (subset? (second t-d) sample))
               theorem-deps)))

(define (conjectures-for-sample-wrapper)
  (define sample
    (map decode-name (read-benchmark (getenv "SAMPLED_NAMES"))))

  (write-json
   (fix-json-for-output (conjectures-from-sample
                         (parse-json-equations (port->string)) sample))))

;; Return equational theorems (filenames, one per file) which would be possible
;; to discover given what's in the provided sample. In other words, those
;; theorems which are equations, and whose dependencies are a subset of the
;; sample.
(define (equations-admitted-by-sample sample)
  (filter (lambda (thm)
            (not (empty? (theorem-to-equation thm))))
          (conjectures-admitted-by sample)))

;; Check if an expression represents an equation in normal form. Normal form
;; requires the equal expressions to be in lexicographic order, and for the
;; variable indices of both, when arranged in post-order of their first
;; occurrence, to count up sequentially from 0.
(define (equation? expr)
  (match expr
    [(list '~= lhs rhs) (and (expression? lhs)
                             (expression? rhs)
                             (lex<=? lhs rhs)
                             (canonical-variables? expr))]
    [_ #f]))

(module+ test
  (def-test-case "Equations"
    (check-true  (equation? '(~= (apply (constant f "Int -> Int")
                                        (variable 0 "Int"))
                                 (apply (constant g "Bool -> Int")
                                        (variable 0 "Bool"))))
                 "Valid equation accepted")

    (check-false (equation? '(~= (apply (constant b "Int -> Int")
                                        (variable 0 "Int"))
                                 (apply (constant a "Bool -> Int")
                                        (variable 0 "Bool"))))
                 "Reject expressions in non-lexicographical order")

    (check-false (equation? '(~= (apply (constant f "Int -> Int")
                                        (variable 1 "Int"))
                                 (apply (constant g "Bool -> Int")
                                        (variable 1 "Bool"))))
                 "Reject variables not starting from 0")

    (check-false (equation? '(~= (apply (apply (constant f "Int -> Int -> Bool")
                                               (variable 1 "Int"))
                                        (variable 0 "Int"))
                                 (apply (constant g "Int -> Bool")
                                        (variable 0 "Int"))))
                 "Reject variables in the wrong order")))

;; Check if an equation's variable indices are in canonical order
(define (canonical-variables? eq)
  (match eq
    [(list '~= lhs rhs) (foldl (lambda (type so-far)
                                 (and so-far
                                      (canonical-variables-for-type? eq type)))
                               #t
                               (remove-duplicates
                                (append (all-variable-types-of lhs)
                                        (all-variable-types-of rhs))))]))

;; Whether the variables of the given type, appearing in the given equation,
;; have their indices in canonical order
(define (canonical-variables-for-type? eq type)
  (define indices (indices-of eq type))
  (equal? indices (range (length indices))))

;; All variable indices of the given type which occur in the given equation, in
;; post-order of their first occurrence
(define (indices-of eq type)
  (match eq
    [(list ~= lhs rhs) (remove-duplicates
                        (append (all-indices-of lhs type)
                                (all-indices-of rhs type)))]))

;; All variable indices of the given type which occur in an expression
(define (all-indices-of expr type)
  (match expr
    [(list 'variable index t) (if (equal? t type)
                                  (list index)
                                  '())]
    [(list 'apply lhs rhs)    (append (all-indices-of lhs type)
                                      (all-indices-of rhs type))]
    [(cons x y)               (append (all-indices-of x   type)
                                      (all-indices-of y   type))]
    [_                        '()]))

;; All of the types which have variables in the given expression
(define (all-variable-types-of expr)
  (remove-duplicates
   (match expr
     [(list 'variable _ type) (list type)]
     [(list 'apply lhs rhs)   (append (all-variable-types-of lhs)
                                      (all-variable-types-of rhs))]
     [_                       '()])))

;; Check if a Racket expression encodes an expression (as used in equations)
(define (expression? expr)
  (match expr
    [(list 'constant name type)  (and (symbol? name)
                                      (string? type))]
    [(list 'variable index type) (and (integer? index)
                                      (>= index 0)
                                      (string? type))]
    [(list 'apply lhs rhs)       (and (expression? lhs)
                                      (expression? rhs))]
    [_                           #f]))

;; Try to convert the normalised theorem from the given file into an equation.
;; Returns a list of results, i.e. containing a single element upon success or
;; empty upon failure.
(define (equation-from f)
  (theorem-to-equation (normed-theorem-of f)))

(module+ test
  (def-test-case "Read equations"
    (check-equal? (equation-from (testing-file
                                  "grammars/packrat_unambigPackrat.smt2"))
                  '()
                  "Theorem which isn't equation doesn't get converted")

    (check-true (list? (map testing-file
                            '("grammars/packrat_unambigPackrat.smt2"
                              "isaplanner/prop_44.smt2"
                              "isaplanner/prop_01.smt2"
                              "isaplanner/prop_15.smt2")))
                "Files containing normal forms are included")
    (check-equal? (equation-from
                    (testing-file "isaplanner/prop_84.smt2"))

                  `((~=
                     (apply (apply (constant ,(nn 'grammars/packrat_unambigPackrat.smt2append) "unknown")
                                   (apply (apply (constant ,(nn 'isaplanner/prop_44.smt2zip) "unknown")
                                                 (apply (apply (constant ,(nn 'isaplanner/prop_01.smt2take) "unknown")
                                                               (apply (constant ,(nn 'isaplanner/prop_15.smt2len) "unknown")
                                                                      (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                                                        (variable 0 "(grammars/packrat_unambigPackrat.smt2list a)")))
                                          (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                            (apply (apply (constant ,(nn 'isaplanner/prop_44.smt2zip) "unknown")
                                          (apply (apply (constant ,(nn 'isaplanner/prop_01.smt2drop) "unknown")
                                                        (apply (constant ,(nn 'isaplanner/prop_15.smt2len) "unknown")
                                                               (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                                                 (variable 0 "(grammars/packrat_unambigPackrat.smt2list a)")))
                                   (variable 1 "(grammars/packrat_unambigPackrat.smt2list b)")))

                     (apply (apply (constant ,(nn 'isaplanner/prop_44.smt2zip) "unknown")
                                   (variable 0 "(grammars/packrat_unambigPackrat.smt2list a)"))
                            (apply (apply (constant ,(nn 'grammars/packrat_unambigPackrat.smt2append) "unknown")
                                          (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)"))
                                   (variable 1 "(grammars/packrat_unambigPackrat.smt2list b)")))))

                  "Theorem which is equation gets converted")

    (for-each (lambda (f)
                (check-true (< (length (equation-from f)) 2)
                            "Extracting equations doesn't crash"))
              (theorem-ids))

    (for-each (lambda (f)
                (define thm (normed-theorem-of f))

                (define eqs (equation-from f))

                (define (strip-args expr)
                  (match expr
                    [(list 'forall vars body) body]
                    [(list 'lambda vars body) body]
                    [(cons x y)               (cons (strip-args x)
                                                    (strip-args y))]
                    [x                        x]))

                (define seems-valid
                  (cond
                    ;; Equations must contain =
                    [(not (member '= (flatten thm))) #f]

                    ;; If we see a '=> before a '= then *either* the equation is
                    ;; conditional, *or* there's a function type somewhere in
                    ;; the arguments. We strip off arguments functions to
                    ;; discard this latter case.
                    [(let ([syms (flatten (strip-args thm))])
                       (and (member 'custom-=> syms)
                            (> (length (member 'custom-=> syms))
                               (length (member '=  syms))))) #f]

                    [else #t]))

                (with-check-info
                  (('f           f)
                   ('thm         thm)
                   ('eqs         eqs)
                   ('seems-valid seems-valid))
                  (check-equal? (length eqs)
                                (if seems-valid 1 0)
                                "Can extract equations from unconditional =")))
              (theorem-ids))))

;; Turns a list, such as '(f x y) into nested unary applications, like
;; '(apply (apply f x) y)
(define (insert-applies lst)
  (match lst
    [(list 'variable _ _) lst]
    [(list 'constant _ _) lst]
    [(list 'apply    _ _) lst]
    ['()                  lst]
    [(list x)             (list x)]
    [(cons x '())         (list x)]
    [(list f x)           (list 'apply f x)]
    [(? list?)            (insert-applies (cons (list 'apply (first lst)
                                                      (second lst))
                                                (rest (rest lst))))]))

;; Converts a TIP expression into one suitable for use in an equation
(define (to-expression x)
  ;; Turns a list '((a) (b) (c) ...) into '((a b c ...)). If any of the lists is
  ;; empty, we return an empty list as a result. Lets us use empty/singleton
  ;; lists like optional values.
  (define (concat-first-elements acc lst)
    (if (empty? lst)
        (list acc)
        (if (empty? (first lst))
            '()
            (concat-first-elements (append acc (first lst))
                                   (rest lst)))))

    (match x
      [(list '= _ _)            '()]
      [(list 'lambda vars body) (to-expression (make-variables vars body))]
      [(list 'variable _ _)     (list x)]
      [(list 'constant _ _)     (list x)]
      [(list 'apply    _ _)     (list x)]

      ;; Assume that any other symbol is a constant. We don't handle types yet,
      ;; so no need to bother inferring one.
      [(? symbol?)              (list (list 'constant x "unknown"))]

      ;; Catch-all for lists; try to convert all elements into expressions, if
      ;; that succeeds then collect up the results and insert-applies.
      [(? list?)                (match (concat-first-elements
                                        '() (map to-expression x))
                                  ['() '()]
                                  [(list exprs) (list (insert-applies exprs))])]))

(define (next-index-for body type)
  (define indices
    (filter integer? (all-indices-of body type)))

  (if (empty? indices)
      0
      (+ 1 (apply max indices))))

;; Replaces occurrences of the variables VARS in BODY with variables suitable
;; for use in an equation
(define (make-variables vars body)
  (foldl (lambda (var body)
           (define type
             (format "~s" (second var)))

           (define idx
             (next-index-for body type))

           (replace-in (first var)
                       (list 'variable idx type)
                       body))
         body
         vars))

;; To to convert the given theorem expression into an equation. Returns an empty
;; list on failure, or a single-element list on success.
(define (theorem-to-equation expr)
  (match expr
    ;; Unwrap assert-not
    [(list 'assert-not thm)   (theorem-to-equation thm)]

    ;; Replace occurrences of universally-quantified variables with variable
    ;; expressions
    [(list 'forall vars body) (theorem-to-equation (make-variables vars body))]

    ;; We don't currently handle types, so we can strip off type-level variables
    [(list 'par _ body)       (theorem-to-equation body)]

    ;; We've found an equation, convert the inner terms
    [(list '= lhs rhs) (let ([x (to-expression lhs)]
                             [y (to-expression rhs)])
                         (if (or (empty? x) (empty? y))
                             '()
                             (if (lex<=? (first x) (first y))
                                 (list (list '~= (first x) (first y)))
                                 (list (list '~= (first y) (first x))))))]

    ;; Non-equations; some of these could be solved by, e.g., an SMT solver

    ;; Implications/conditional statements
    [(list '=> _ _) '()]

    ;; Negation
    [(list 'not _) '()]

    ;; Inequalities
    [(list 'distinct _ _) '()]

    ;; Catch-all for theorems which call arbitrary functions/constants; we avoid
    ;; matching native symbols, since we don't want to swallow up potentially
    ;; translatable theorems
    [(? (lambda (x)
          (and (list? x)
               (symbol? (first x))
               (not (member (first x) native-symbols))))) '()]

    [x (error (string-append "Unhandled case: " (~a x)))]))

;; Try to parse the given string as a JSON representation of an equation, e.g.
;; from reduce-equations. Returns a list containing the result on success, or an
;; empty list on failure.
(define/test-contract (parse-json-equation str)
  (-> string? (or/c (list/c equation?)
                    empty?))
  (with-handlers ([exn:fail:read? (lambda (e) '())])
    (parse-equation (string->jsexpr str))))

(module+ test
  (def-test-case "Parse JSON"
    (define json-eq
      "{
         \"relation\": \"~=\",
         \"lhs\": {
           \"role\": \"application\",
           \"lhs\": {
             \"role\": \"application\",
             \"lhs\": {
               \"role\": \"constant\",
               \"type\": \"Nat -> Nat -> Nat\",
               \"symbol\": \"plus\"
             },
             \"rhs\": {
               \"role\": \"variable\",
               \"type\": \"Nat\",
               \"id\": 1
             }
           },
           \"rhs\": {
             \"role\": \"variable\",
             \"type\": \"Nat\",
             \"id\": 0
           }
         },
         \"rhs\": {
           \"role\": \"application\",
           \"lhs\": {
             \"role\": \"application\",
             \"lhs\": {
               \"role\": \"constant\",
               \"type\": \"Nat -> Nat -> Nat\",
               \"symbol\": \"plus\"
             },
             \"rhs\": {
               \"role\": \"variable\",
               \"type\": \"Nat\",
               \"id\": 0
             }
           },
           \"rhs\": {
             \"role\": \"variable\",
             \"type\": \"Nat\",
             \"id\": 1
           }
         }
       }")

    (define parsed
      (parse-json-equation json-eq))

    ;; We switch the lhs and rhs, so they're in lexicographic order
    (define expected
      '(~= (apply (apply (constant plus "Nat -> Nat -> Nat")
                         (variable 0 "Nat"))
                  (variable 1 "Nat"))
           (apply (apply (constant plus "Nat -> Nat -> Nat")
                         (variable 1 "Nat"))
                  (variable 0 "Nat"))))

    (with-check-info
      (('expected expected)
       ('parsed   parsed))

      (check-true (and (list? parsed)
                       (not (empty? parsed))
                       (equation? (first parsed)))
                  "Parsing JSON gives an equation")

      (check-equal? parsed (list expected)
                    "Parsing gives expected value"))

    (define test-eqs
      (file->string (getenv "TEST_DATA")))

    (check-true  (jsexpr? (string->jsexpr test-eqs)))
    (check-true  (list?   (string->jsexpr test-eqs)))
    (check-false (empty?  (string->jsexpr test-eqs)))

    (for-each (lambda (obj)
                (check-pred equation? obj))
              (parse-json-equations test-eqs))))

(define/test-contract (parse-equation raw-eq)
  (-> jsexpr? (or/c (list/c equation?)
                    empty?))

  ;; Cause a read error, which we'll turn into an empty result
  (define (fail msg . args)
    (define str
      (if (empty? args)
          msg
          (apply format (cons msg args))))

    (raise (exn:fail:read str
                          (current-continuation-marks)
                          '())))

  ;; Look up the value of K in hash table OBJ, or else fail
  (define (get-key obj k)
    (unless (and (hash? obj)
                 (hash-has-key? obj k))
      (fail "Couldn't find key ~s" k))
    (hash-ref obj k (lambda () (fail "Error getting key ~s" k))))

  ;; Parse JSON structure to match s-expression equations
  (define (json-to-expr obj)
    (match (get-key obj 'role)
      ["application" `(apply ,(json-to-expr (get-key obj 'lhs))
                             ,(json-to-expr (get-key obj 'rhs)))]
      ["variable"    `(variable ,(get-key obj 'id)
                                ,(get-key obj 'type))]
      ["constant"    `(constant ,(string->symbol (get-key obj 'symbol))
                                ,(get-key obj 'type))]
      [x             (fail "Unknown role ~s" x)]))

  ;; Catch read exceptions from string->jsexpr, but also lets us short-circuit
  ;; when we find a problem with the input.
  (with-handlers ([exn:fail:read? (lambda (e) '())])
    (unless (equal? (get-key raw-eq 'relation) "~=")
      (fail "Not an equation object"))

    (define raw-lhs
      (get-key raw-eq 'lhs))

    (define raw-rhs
      (get-key raw-eq 'rhs))

    (define lhs
      (json-to-expr raw-lhs))

    (define rhs
      (json-to-expr raw-rhs))

    (list (make-normal-equation lhs rhs))))

(define (make-normal-equation lhs rhs)

  ;; Re-numbers the variables in an equation to count 0, 1, 2, ...
  (define (renumber eq)
    ;; Loop through each variable type, renumbering variables of that type
    (foldl (lambda (type eq)
             ;; Replace all variables of this type with temporary values, to
             ;; avoid having mixtures of old and new indices
             (define temp
               (foldl (lambda (idx eq)
                        (replace-in `(variable ,idx                    ,type)
                                    `(variable ,(format "temp-~a" idx) ,type)
                                    eq))
                      eq
                      (indices-of eq type)))

             ;; Replace temporary values with sequential numbers
             (foldl (lambda (temp-var eq)
                      (replace-in `(variable ,temp-var ,type)
                                  `(variable ,(next-index-for eq type) ,type)
                                  eq))
                    temp
                    (indices-of temp type)))
           eq
           (append (all-variable-types-of (second eq))
                   (all-variable-types-of (third  eq)))))

  (define renumbered-1
    (renumber `(~= ,lhs ,rhs)))

  (define renumbered-2
    (renumber `(~= ,rhs ,lhs)))

  (cond
    [(lex<=? (second renumbered-1) (third renumbered-1)) renumbered-1]
    [(lex<=? (second renumbered-2) (third renumbered-2)) renumbered-2]
    [else (error (format "Couldn't sort equation ~s" `(~= lhs rhs)))]))

(define (parse-json-equations str)
  (with-handlers ([exn:fail:read? (lambda (e) '())])
    ;; Parse, then unwrap the equations; if any failed, we fail
    (map (lambda (obj)
           (define eq (parse-equation obj))
           (if (empty? eq)
               (raise (exn:fail:read "Didn't get equation"
                                     (current-continuation-marks)
                                     '()))
               (first eq)))
         (string->jsexpr str))))

;; Turns an equation s-expression into a datastructure which, if given to
;; write-json, will output an equation compatible with mlspec et al.
(define (equation-to-jsexpr eq)
  (define (expression-to-jsexpr x)
    (make-hash (match x
      [(list 'variable id type)  `((role   . "variable")
                                   (id     . ,id)
                                   (type   . ,type))]
      [(list 'constant sym type) `((role   . "constant")
                                   (symbol . ,(symbol->string sym))
                                   (type   . ,type))]
      [(list 'apply lhs rhs)     `((role   . "application")
                                   (lhs    . ,(expression-to-jsexpr lhs))
                                   (rhs    . ,(expression-to-jsexpr rhs)))])))

  (match eq
    [(list '~= lhs rhs)
     (make-hash `((relation . "~=")
                  (lhs      . ,(expression-to-jsexpr lhs))
                  (rhs      . ,(expression-to-jsexpr rhs))))]))

(define/test-contract (equations-match? x y)
  (-> equation? equation? boolean?)

  (define-values (x-l x-r)
    (match x
      [(list '~= l r) (values l r)]))

  (define-values (y-l y-r)
    (match y
      [(list '~= l r) (values l r)]))

  (and (expressions-match? x-l y-l)
       (expressions-match? x-r y-r)))

(module+ test
    (def-test-case "Equation matching"
    (check-true (equations-match? '(~= (constant bar "foo")
                                       (variable 0   "foo"))
                                  '(~= (constant bar "foo")
                                       (variable 0   "foo")))
                "Identical equations match")

    (check-true (equations-match? '(~= (apply (constant func "unknown")
                                              (variable 0    "foo"))
                                       (constant bar "foo"))
                                  '(~= (apply (constant func "baz")
                                              (variable 0    "foo"))
                                       (constant bar "foo")))
                "Constant types don't affect match")

    (check-false (equations-match? '(~= (apply (constant bar "foo")
                                               (variable 0   "baz"))
                                        (variable 0   "baz"))
                                   '(~= (apply (constant bar "foo")
                                               (variable 0   "baz"))
                                        (variable 1   "baz")))
                 "Different indices don't match")

    (check-false (equations-match? '(~= (constant bar "foo")
                                        (variable 0   "foo"))
                                   '(~= (constant bar "foo")
                                        (variable 0   "baz")))
                 "Different variable types don't match")

    (check-false (equations-match? '(~= (constant bar "foo")
                                        (variable 0   "foo"))
                                   '(~= (constant baz "foo")
                                        (variable 0   "foo")))
                 "Different constant names don't match")

    (check-false (equations-match? '(~= (apply (constant baz "unknown")
                                               (variable 0   "foo"))
                                        (constant bar "foo"))
                                   '(~= (apply (variable 0   "Int -> Bool")
                                               (variable 0   "foo"))
                                        (constant bar "foo")))
                 "Different structures don't match")))

(define/test-contract (expressions-match? x y)
  (-> expression? expression? boolean?)

  (match (list x y)
    [(list (list 'variable index1 type1)
           (list 'variable index2 type2))
     (and (equal? index1 index2)
          (equal? type1  type2))]

    ;; We don't currently infer types for TIP constants, so many will be
    ;; "unknown"; since overloading isn't allowed, we can rely on the names
    ;; being unique and ignore the types.
    [(list (list 'constant name1 _)
           (list 'constant name2 _))
     (equal? name1 name2)]

    [(list (list 'apply f1 x1)
           (list 'apply f2 x2))
     (and (expressions-match? f1 f2)
          (expressions-match? x1 x2))]

    [_ #f]))

(define (equations-from-list lst)
  (map (lambda (thm)
         (equation-to-jsexpr
          (first
           (theorem-to-equation thm))))
       lst))

(define (eqs-to-json-wrapper)
  (write-json (equations-from-list (read-benchmark (port->string)))))

(define (precision-recall-eqs-wrapper incoming-json truth-source g-truth)
  (define from-json
    (parse-json-equations incoming-json))

  (define ground-truth
    (map (lambda (thm)
           (list truth-source thm))
         (read-benchmark (file->string g-truth))))

  (fix-json-for-output
   (conjectures-from-raw from-json
                         (find-eqs-intersection-raw from-json ground-truth))))

(define (fix-json-for-output jsexpr)
  (hash-update jsexpr
               'wanted
               (lambda (wanted)
                 (map (lambda (entry)
                        (hash-update (hash-remove entry 'theorem)
                                     'equation
                                     (lambda (x)
                                       (map equation-to-jsexpr x))))
                      wanted))))
