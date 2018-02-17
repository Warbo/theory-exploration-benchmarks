#lang racket

(require json)
(require lib/compare)
(require lib/lists)
(require lib/normalise)
(require lib/sampling)
(require lib/theorems)
(require lib/tip)
(require lib/util)

(provide conjectures-admitted-by-sample-wrapper
         conjectures-for-sample-wrapper equation-to-jsexpr eqs-to-json-wrapper
         parse-json-equation parse-json-equations precision-recall-eqs-wrapper)

(module+ test
  (require lib/testing))

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
              (quick-or-full (take (shuffle (theorem-ids)) 10)
                             (theorem-ids)))))

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
  (map first
       (filter (lambda (t-d)
                 (subset? (second t-d) sample))
               (theorem-deps))))

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
    (check-true  (equation? '(~= (apply (constant f      "Int -> Int")
                                        (variable free 0 "Int"))
                                 (apply (constant g      "B ool -> Int")
                                        (variable free 1 "Bool"))))
                 "Valid equation accepted")

    (check-false (equation? '(~= (apply (constant b      "Int -> Int")
                                        (variable free 0 "Int"))
                                 (apply (constant a      "Bool -> Int")
                                        (variable free 1 "Bool"))))
                 "Reject expressions in non-lexicographical order")

    (check-false (equation? '(~= (apply (constant f      "Int -> Int")
                                        (variable free 1 "Int"))
                                 (apply (constant g      "Bool -> Int")
                                        (variable free 0 "Bool"))))
                 "Reject variables not starting from 0")

    (check-false (equation? '(~= (apply (constant f      "Int -> Int")
                                        (variable free 0 "Int"))
                                 (apply (constant g      "Bool -> Int")
                                        (variable free 0 "Bool"))))
                 "Reject variables with mismatched types")

    (check-false (equation? '(~= (apply (apply (constant f "Int -> Int -> Bool")
                                               (variable free 1 "Int"))
                                        (variable free 0 "Int"))
                                 (apply (constant g      "Int -> Bool")
                                        (variable free 0 "Int"))))
                 "Reject variables in the wrong order")))

;; Check if an equation's variable indices are in canonical order
(define (canonical-variables? eq)
  (match eq
    [(list '~= lhs rhs)
     (let* ((vars        (remove-duplicates (append (free-variables-in lhs)
                                                    (free-variables-in rhs))))
            (types-match (foldl (lambda (var so-far)
                                  (match (list var so-far)
                                    [(list (list 'variable 'free index type)
                                           (list ok types))
                                     (let ((found (hash-ref types index type)))
                                       (list (and ok (equal? type found))
                                             (hash-set types index type)))]))
                                (list #t (hash))
                                vars))
            (in-order    (equal? (map (lambda (var)
                                        (match var
                                          [(list 'variable 'free index _) index]))
                                      vars)
                                 (range 0 (length vars)))))
       (and in-order
            (first types-match)))]))

(define (free-variables-in expr)
  (remove-duplicates
   (match expr
     [(list 'variable 'free _ _) (list expr)]
     [(list 'apply lhs rhs)      (append (free-variables-in lhs)
                                         (free-variables-in rhs))]
     [(list '~= lhs rhs)         (append (free-variables-in lhs)
                                         (free-variables-in rhs))]
     [_                          '()])))

;; Check if a Racket expression encodes an expression (as used in equations)
(define (expression? expr)
  (match expr
    [(list 'constant name type)       (and (symbol? name)
                                           (string? type))]
    [(list 'variable kind index type) (and (member kind '(free bound))
                                           (integer? index)
                                           (>= index 0)
                                           (string? type))]
    [(list 'apply lhs rhs)            (and (expression? lhs)
                                           (expression? rhs))]
    [_                                #f]))

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
                                                                      (variable free 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                                                        (variable free 1 "(grammars/packrat_unambigPackrat.smt2list a)")))
                                          (variable free 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                            (apply (apply (constant ,(nn 'isaplanner/prop_44.smt2zip) "unknown")
                                          (apply (apply (constant ,(nn 'isaplanner/prop_01.smt2drop) "unknown")
                                                        (apply (constant ,(nn 'isaplanner/prop_15.smt2len) "unknown")
                                                               (variable free 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                                                 (variable free 1 "(grammars/packrat_unambigPackrat.smt2list a)")))
                                   (variable free 2 "(grammars/packrat_unambigPackrat.smt2list b)")))

                     (apply (apply (constant ,(nn 'isaplanner/prop_44.smt2zip) "unknown")
                                   (variable free 1 "(grammars/packrat_unambigPackrat.smt2list a)"))
                            (apply (apply (constant ,(nn 'grammars/packrat_unambigPackrat.smt2append) "unknown")
                                          (variable free 0 "(grammars/packrat_unambigPackrat.smt2list b)"))
                                   (variable free 2 "(grammars/packrat_unambigPackrat.smt2list b)")))))

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
    [(list 'variable _ _ _) lst]
    [(list 'constant _ _)   lst]
    [(list 'apply    _ _)   lst]
    ['()                    lst]
    [(list x)               (list x)]
    [(cons x '())           (list x)]
    [(list f x)             (list 'apply f x)]
    [(? list?)              (insert-applies (cons (list 'apply (first lst)
                                                        (second lst))
                                                  (rest (rest lst))))]))

;; Replace (lambda ((arg1 "type1") (arg2 "type2") ...) body) with
;; (lambda ((arg1 "type1")) (lambda ((arg2 "type2")) (... body)))
(define (curry-lambdas expr)
  (match expr
    [(list 'lambda args body)
     (match args
       [(list)          (curry-lambdas body)]
       [(cons arg rest) `(lambda (,arg)
                           ,(curry-lambdas `(lambda ,rest ,body)))])]
    [(cons x y) (cons (curry-lambdas x) (curry-lambdas y))]
    [_          expr]))

(module+ test
  (def-test-case "Can curry lambdas"
    (let ((expr '(variable free 42 "t")))
      (check-equal? (curry-lambdas expr) expr "Variables don't get curried"))

    (let ((expr '(constant foo "t")))
      (check-equal? (curry-lambdas expr) expr "Constants don't get curried"))

    (let ((expr '(apply (constant bar "unknown") (variable bound 0 "x"))))
      (check-equal? (curry-lambdas expr) expr "Apply doesn't get curried"))

    (let* ((body '(variable free 5 "t"))
           (expr `(lambda () ,body)))
      (check-equal? (curry-lambdas expr) body "Nullary gets unwrapped"))

    (let* ((body '(variable free 0 "t"))
           (expr `(lambda ((x "t1") (y "t2") (z "t3")) ,body)))
      (check-equal? (curry-lambdas expr)
                    `(lambda ((x "t1"))
                       (lambda ((y "t2"))
                         (lambda ((z "t3"))
                           ,body)))
                    "Multi-arg functions get curried"))))

(define (lookup-env env symbol)
  (define (go n remaining)
    (match remaining
      ['()                          '()]
      [(cons (list name type) rest) (if (equal? name symbol)
                                        (list n type)
                                        (go (+ 1 n) rest))]))

  (go 0 env))

(module+ test
  (def-test-case "Can lookup bound variables from environment"
    (check-equal? (lookup-env '() 'foo)
                  '()
                  "No result for empty env")

    (check-equal? (lookup-env '((bar "type1")) 'foo)
                  '()
                  "No result for missing var")

    (check-equal? (lookup-env '((foo "type1")) 'foo)
                  '(0 "type1")
                  "Find singleton var")

    (check-equal? (lookup-env '((foo "type1") (bar "type2") (baz "type3")) 'bar)
                  '(1 "type2")
                  "Find buried var")))

(define (bind-vars env expr)
  ;; Recurse down an expression tree, replacing occurrences of anything in env
  ;; with bound, de Bruijn variables. We extend env when walking under lambdas.
  (match expr
    ;; We only work with single-argument (curried) functions
    [(list 'lambda (list arg) body) `(lambda ,(bind-vars (cons arg env) body))]

    [(list 'lambda '() body) (error "Nullary function; currying problem?")]
    [(list 'lambda _   body) (error "N-ary function; currying problem?")]

    ;; Look up symbols in env
    [(? symbol?) (match (lookup-env env expr)
                   ;; Not found, use as-is
                   ['() expr]

                   ;; Found, replace with a bound variable
                   [(list n type) `(variable bound ,n ,type)])]

    [(cons x y) (cons (bind-vars env x) (bind-vars env y))]
    [_          expr]))

(module+ test
  (def-test-case "Bind lambda args"
    (check-equal? (bind-vars '() '(lambda ((arg1 "type1"))
                                    (foo (cons arg1 nil))))
                  '(lambda
                       (foo (cons (variable bound 0 "type1") nil)))
                  "Bind single lambda arg")

    (check-equal? (bind-vars '() '(lambda ((arg1 "type1"))
                                    (foo arg1 (lambda ((arg2 "type2"))
                                                (bar arg1 arg2)))))
                  '(lambda
                       (foo (variable bound 0 "type1")
                            (lambda
                                (bar (variable bound 1 "type1")
                                     (variable bound 0 "type2")))))
                  "de Bruijn indices match up")

    (check-equal? (bind-vars '() '(lambda ((arg1 "type1"))
                                    (foo arg1
                                         (lambda ((arg1 "type2"))
                                           (bar arg1 arg1 (lambda ((arg1 "type1"))
                                                            (baz arg1))))
                                         arg1)))
                  '(lambda
                       (foo (variable bound 0 "type1")
                            (lambda
                                (bar (variable bound 0 "type2")
                                     (variable bound 0 "type2")
                                     (lambda
                                         (baz (variable bound 0 "type1")))))
                            (variable bound 0 "type1")))
                  "Shadowing gets correct indices")

    (check-equal? (bind-vars '((arg1 "type1")) '(lambda ((arg2 "type2"))
                                                  (foo arg2 arg1)))
                  '(lambda
                       (foo (variable bound 0 "type2")
                            (variable bound 1 "type1")))
                  "Variables taken from env")))

;; Converts a TIP expression into one suitable for use in an equation
(define (to-expression expr)
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

  (define (go x)
    (match x
      [(list '=        _ _)   '()     ]
      [(list 'variable _ _ _) (list x)]
      [(list 'constant _ _)   (list x)]
      [(list 'apply    _ _)   (list x)]

      ;; We should have already curried and bound lambdas, but their bodies still
      ;; need converting to expressions.
      [(list 'lambda body) (map (curry cons 'lambda) (go body))]

      ;; If we find a lambda with an argument list, that means we've not
      ;; pre-processed the lambdas correctly
      [(list 'lambda args body)
       (error "Hit unprocessed lambda; bug in currying/binding?")]

      ;; (@ f x) is equivalent to (f x), so avoid the indirection. If any
      ;; 2-lisps need such indirection, like Common Lisp's funcall, they can
      ;; infer it as needed by e.g. spotting when the lhs of an apply is a
      ;; variable.
      [(list '@ a b) (go (list a b))]

      ;; Assume that any other symbol is a constant. We don't handle types yet,
      ;; so no need to bother inferring one.
      [(? symbol?) (list (list 'constant x "unknown"))]

      ;; Catch-all for lists; try to convert all elements into expressions, if
      ;; that succeeds then collect up the results and insert-applies.
      [(? list?) (match (concat-first-elements
                         '() (map go x))
                   ['() '()]
                   [(list exprs) (list (insert-applies exprs))])]

      ;; Catch all, for things like numbers
      [_ x]))

  (go (bind-vars '() (curry-lambdas expr))))

(module+ test
  (def-test-case "Check expression lambdas"
    (define expr
      '(lambda ((x a)) (bind (@ f x) g)))

    (check-equal? (to-expression expr)
                  '((lambda (apply (apply (constant bind "unknown")
                                          (apply (constant f "unknown")
                                                 (variable bound 0 "a")))
                                   (constant g "unknown"))))))

  (def-test-case "Check expression @s"
    (define expr
      (make-variables '((x a) (f (=> a (list b))))
                      '(foo (@ f x))))

    (check-equal? (to-expression expr)
                  '((apply (constant foo "unknown")
                           (apply (variable free 1 "(=> a (list b))")
                                  (variable free 0 "a")))))))

;; Replaces occurrences of the variables VARS in BODY with variables suitable
;; for use in an equation
(define (make-variables vars body)
  (foldl (lambda (var body)
           (define type
             (format "~s" (second var)))

           (define idx
             (next-var-index body))

           (replace-in (first var)
                       (list 'variable 'free idx type)
                       body))
         body
         vars))

(define (next-var-index expr)
  (match expr
    [(list '~=    lhs rhs)     (max (next-var-index lhs)
                                    (next-var-index rhs))]
    [(list 'apply lhs rhs)     (max (next-var-index lhs)
                                    (next-var-index rhs))]
    [(list 'variable free i _) (+ 1 i)]
    [(cons x y)                (max (next-var-index x)
                                    (next-var-index y))]
    [_                         0]))

;; Try to convert the given theorem expression into an equation. Returns an
;; empty list on failure, or a single-element list on success.
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
                         (match (list x y)
                           [(list '() _) '()]
                           [(list _ '()) '()]
                           [(list (list x) (list y))
                            (list (make-normal-equation x y))]))]

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

(module+ test
  (def-test-case "Check theorem lambdas"
    (define thm1
      '(assert-not
        (par (a b c)
             (forall ((m (list a)) (f (=> a (list b))) (g (=> b (list c))))
                     (= (bind (bind m f) g)
                        (bind m (lambda ((x a)) (bind (@ f x) g))))))))

    (define thm2
      '(assert-not
        (par (a)
             (forall ((xs (list a)))
                     (= (dropWhile (lambda ((x a)) false) xs) xs)))))

    (check-equal? (theorem-to-equation thm1) '())
    (check-equal? (theorem-to-equation thm2) '()))

  (def-test-case "Check theorem @s"
    (define thm
      '(assert-not
        (par (a b)
             (forall ((x a) (f (=> a (list b))))
                     (= (bind (return x) f) (@ f x))))))

    ;; Sides swapped due to lexical ordering
    (check-equal? (theorem-to-equation thm)
                  '((~= (apply (apply (constant bind "unknown")
                                      (apply (constant return "unknown")
                                             (variable free 0 "a")))
                               (variable free 1 "(=> a (list b))"))
                        (apply (variable free 1 "(=> a (list b))")
                               (variable free 0 "a"))))))

  (def-test-case "Check theorem sorting"
    (check-equal? (theorem-to-equation '(assert-not
                                         (forall ((x t1) (y t2)) (= x y))))
                  '((~= (variable free 0 "t1")
                        (variable free 1 "t2")))
                  "Renumbered variables ordered by type")

    (check-equal? (theorem-to-equation '(assert-not
                                         (forall ((x t2) (y t1)) (= x y))))
                  '((~= (variable free 0 "t1")
                        (variable free 1 "t2")))
                  "Sides switched into order and variables renumbered")))

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
                         (variable free 0 "Nat"))
                  (variable free 1 "Nat"))
           (apply (apply (constant plus "Nat -> Nat -> Nat")
                         (variable free 1 "Nat"))
                  (variable free 0 "Nat"))))

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
      ["variable"    `(variable ,(if (and (hash-has-key? obj 'bound)
                                          (get-key obj 'bound))
                                     'bound
                                     'free)
                                ,(get-key obj 'id)
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

;; Re-numbers the free variables in an equation to count 0, 1, 2, ...
(define (renumber eq)
  ;; Replace free variables with temporary values, to avoid having mixtures of
  ;; old and new indices
  (define temp
    (foldl (lambda (var eq)
             (match var
               [(list 'variable 'free index type)
                (replace-in var
                            (list 'variable 'free (format "temp-~a" index) type)
                            eq)]))
           eq
           (remove-duplicates (free-variables-in eq))))

  ;; Replace temporary values with sequential numbers
  (first (foldl (lambda (temp-var result)
                  (match (list temp-var result)
                    [(list (list 'variable 'free index type)
                           (list eq next))
                     (list (replace-in temp-var
                                       (list 'variable 'free next type)
                                       eq)
                           (+ 1 next))]))
                (list temp 0)
                (remove-duplicates (free-variables-in temp)))))

(define (make-normal-equation lhs rhs)
  (define renumbered-1
    (renumber `(~= ,lhs ,rhs)))

  (define renumbered-2
    (renumber `(~= ,rhs ,lhs)))

  (if (lex<=? (second renumbered-1) (second renumbered-2))
      renumbered-1
      renumbered-2))

(module+ test
  (def-test-case "Normalised equations"
    (check-equal? (make-normal-equation '(variable free 3 "Bool")
                                        '(apply (constant foo "unknown")
                                                (variable free 2 "Int")))
                  '(~= (apply (constant foo "unknown")
                              (variable free 0 "Int"))
                       (variable free 1 "Bool")))))

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
      [(list 'variable kind id type) `((role   . "variable")
                                       (id     . ,id)
                                       (type   . ,type)
                                       (bound  . ,(equal? kind 'bound)))]
      [(list 'constant sym type)     `((role   . "constant")
                                       (symbol . ,(symbol->string sym))
                                       (type   . ,type))]
      [(list 'apply lhs rhs)         `((role   . "application")
                                       (lhs    . ,(expression-to-jsexpr lhs))
                                       (rhs    . ,(expression-to-jsexpr rhs)))])))

  (match eq
    [(list '~= lhs rhs)
     (make-hash `((relation . "~=")
                  (lhs      . ,(expression-to-jsexpr lhs))
                  (rhs      . ,(expression-to-jsexpr rhs))))]))

(define (strip-cons-des-prefix s)
  (define (strip-prefix upper? pre str)
    (let* ((glob (if upper? "Global" "global"))
           (enc  (string-append "global" (encode16 pre))))
      (cond
        [(string-prefix? str pre) (substring str (string-length pre))]
        [(string-prefix? str enc)
         (string-append glob      (substring str (string-length enc)))]
        [#t str])))

  (if (symbol? s)
      (string->symbol (strip-prefix #f "destructor-"
                                    (strip-prefix #t "constructor-"
                                                  (symbol->string s))))
      s))

(define (cons-des-funcs-to-raw expr)
  (match expr
    [(list '~= lhs rhs)     (map cons-des-funcs-to-raw expr)]
    [(list 'constant f t)   (list 'constant (strip-cons-des-prefix f) t)]
    [(list 'variable _ _ _) expr]
    [(list 'apply f x)      (map cons-des-funcs-to-raw expr)]
    [(cons x y)             (cons (cons-des-funcs-to-raw x)
                                  (cons-des-funcs-to-raw y))]
    [_                      expr]))

(define/test-contract (equations-match? x y)
  (-> equation? equation? boolean?)
  ;; Replace 'constructor-foo' with 'foo' and 'destructor-bar' with 'bar' (even
  ;; if hex encoded), since they're eta-equivalent so should match if needed.
  (match (list (cons-des-funcs-to-raw x)
               (cons-des-funcs-to-raw y))
    [(list (list '~= x-l x-r)
           (list '~= y-l y-r))
     (match (list (make-normal-equation x-l x-r)
                  (make-normal-equation y-l y-r))
       [(list (list '~= xl xr)
              (list '~= yl yr))
        (and (expressions-match? xl yl)
             (expressions-match? xr yr))])]))

(module+ test
  (def-test-case "Equation matching"
    (check-true (equations-match? '(~= (constant bar    "foo")
                                       (variable free 0 "foo"))
                                  '(~= (constant bar    "foo")
                                       (variable free 0 "foo")))
                "Identical equations match")

    (check-true (equations-match? '(~= (apply (constant func   "unknown")
                                              (variable free 0 "foo"))
                                       (constant bar "foo"))
                                  '(~= (apply (constant func   "baz")
                                              (variable free 0 "foo"))
                                       (constant bar "foo")))
                "Constant types don't affect match")

    (check-false (equations-match? '(~= (apply (constant bar    "foo")
                                               (variable free 0 "baz"))
                                        (variable free 0 "baz"))
                                   '(~= (apply (constant bar    "foo")
                                               (variable free 0 "baz"))
                                        (variable free 1 "baz")))
                 "Different indices don't match")

    (check-false (equations-match? '(~= (constant bar    "foo")
                                        (variable free 0 "foo"))
                                   '(~= (constant baz    "foo")
                                        (variable free 0 "foo")))
                 "Different constant names don't match")

    (check-false (equations-match? '(~= (apply (constant baz    "unknown")
                                               (variable free 0 "foo"))
                                        (constant bar "foo"))
                                   '(~= (apply (variable free 0 "Int -> Bool")
                                               (variable free 1 "foo"))
                                        (constant bar "foo")))
                 "Different structures don't match")

    (check-true (equations-match? '(~= (constant bar "t1")
                                       (constant constructor-foo "t1"))
                                  '(~= (constant bar "unknown")
                                       (constant foo "unknown")))
                "Constructors match their expansions")

    (check-true (equations-match? '(~= (constant bar "t1")
                                       (constant destructor-foo "unknown"))
                                  '(~= (constant bar "unknown")
                                       (constant foo "t1")))
                "Destructors match their expansions")

    (check-true (equations-match?
                 '(~= (constant bar "t1")
                      (constant global636f6e7374727563746f722d666f6f "t1"))
                 '(~= (constant Global666f6f "t1")
                      (constant bar "t1")))
                "Encoded constructor-foo matches encoded foo")

    (check-true (equations-match?
                 '(~= (constant bar "t1")
                      (constant global64657374727563746f722d666f6f "t1"))
                 '(~= (constant bar "t1")
                      (constant global666f6f "t1")))
                "Encoded destructor-foo matches encoded foo")

    (check-false (equations-match? '(~= (constant bar "t1")
                                        (constant constructor-foo "t1"))
                                   '(~= (constant bar "t1")
                                        (constant baz "t1")))
                 "Constructor function names must still match")))

(define/test-contract (expressions-match? x y)
  (-> expression? expression? boolean?)

  (match (list x y)
    ;; Variable types should be consistent within an expression (so we can
    ;; number them consistently) but may differ between expressions (e.g. if
    ;; they come from different systems, which may have rewritten or processed
    ;; the types). Hence we ignore the types when matching, and only compare the
    ;; indices and kind (bound/free). This is fine, since we're assuming
    ;; expressions are type-correct, and hence cannot differ *only* by variable
    ;; type (as at least one would be ill typed; or else we're applying one
    ;; variable to another, in which case we can hand-wave it away by saying
    ;; "polymorphism").  Note that we must still compare indices, since
    ;; different variables of the same type can lead to different semantics;
    ;; e.g. (= (plus x y) (plus y x)) is different to (= (plus x x) (plus x x))
    [(list (list 'variable kind1 index1 type1)
           (list 'variable kind2 index2 type2))
     (and (equal? kind1  kind2)
          (equal? index1 index2))]

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
         (read-benchmark g-truth)))

  (fix-json-for-output
   (conjectures-from-raw from-json
                         (find-eqs-intersection-raw from-json ground-truth))))

(module+ test
  (def-test-case "Find matches in example data"
    (begin
      ;; These two should be found to match

      (define want-eq
        (first (theorem-to-equation
                '(assert-not
                  (par (i o) (forall ((a (=> i o)) (b (List i)) (c (List i)))
                                     (= (append (map a b) (map a c))
                                        (map a (append b c)))))))))

      (define found-eq
        (first
         (parse-json-equation
          "{ \"relation\": \"~=\",
             \"lhs\": { \"role\": \"application\",
                        \"lhs\": { \"role\": \"application\",
                                   \"lhs\": { \"role\": \"constant\",
                                              \"type\": \"List Integer -> List Integer -> List Integer\",
                                              \"symbol\": \"append\" },
                                   \"rhs\": { \"role\": \"application\",
                                              \"lhs\": { \"role\": \"application\",
                                                         \"lhs\": { \"role\": \"constant\",
                                                                    \"type\": \"(Integer -> Integer) -> List Integer -> List Integer\",
                                                                    \"symbol\": \"map\" },
                                                         \"rhs\": { \"role\": \"variable\",
                                                                    \"type\": \"Integer -> Integer\",
                                                                    \"id\": 9 } },
                                              \"rhs\": { \"role\": \"variable\",
                                                         \"type\": \"List Integer\",
                                                         \"id\": 3 } } },
                        \"rhs\": { \"role\": \"application\",
                                   \"lhs\": { \"role\": \"application\",
                                              \"lhs\": { \"role\": \"constant\",
                                                         \"type\": \"(Integer -> Integer) -> List Integer -> List Integer\",
                                                         \"symbol\": \"map\" },
                                              \"rhs\": { \"role\": \"variable\",
                                                         \"type\": \"Integer -> Integer\",
                                                         \"id\": 9 } },
                                   \"rhs\": { \"role\": \"variable\",
                                              \"type\": \"List Integer\",
                                              \"id\": 4 } } },
             \"rhs\": { \"role\": \"application\",
                        \"lhs\": { \"role\": \"application\",
                                   \"lhs\": { \"role\": \"constant\",
                                              \"type\": \"(Integer -> Integer) -> List Integer -> List Integer\",
                                              \"symbol\": \"map\" },
                                   \"rhs\": { \"role\": \"variable\",
                                              \"type\": \"Integer -> Integer\",
                                              \"id\": 9 } },
                        \"rhs\": { \"role\": \"application\",
                                   \"lhs\": { \"role\": \"application\",
                                              \"lhs\": { \"role\": \"constant\",
                                                         \"type\": \"List Integer -> List Integer -> List Integer\",
                                                         \"symbol\": \"append\" },
                                              \"rhs\": { \"role\": \"variable\",
                                                         \"type\": \"List Integer\",
                                                         \"id\": 3 } },
                                   \"rhs\": { \"role\": \"variable\",
                                              \"type\": \"List Integer\",
                                              \"id\": 4 } } } }")))

      (with-check-info
        (('want-eq  want-eq)
         ('found-eq found-eq))
        (check-true (equations-match? want-eq found-eq)
                    "Found expected equation")))

    (begin
      (define want-eq2
        '(~= (apply (constant f "Int -> Bool")
                    (variable free 0 "Int"))
             (variable free 1 "Bool")))

      (define found-eq2
        (first (parse-json-equation
                "{\"relation\": \"~=\",
                  \"lhs\": {
                     \"role\": \"application\",
                     \"lhs\":  {
                       \"role\":   \"constant\",
                       \"symbol\": \"f\",
                       \"type\":   \"Int -> Bool\"},
                     \"rhs\":  {
                       \"role\": \"variable\",
                       \"id\":   0,
                       \"type\": \"Int\"}},
                  \"rhs\": {
                    \"role\": \"variable\",
                    \"id\":   0,
                    \"type\": \"Bool\"}}")))

      (with-check-info
        (('want-eq  want-eq2)
         ('found-eq found-eq2))
        (check-true (equations-match? want-eq2 found-eq2)
                    "Parsed vars distinct if types differ, even if ids don't")))

    (begin
      (define result
        (precision-recall-eqs-wrapper
         (file->string (getenv "TEST_LIST_EQS"))
         (getenv "TEST_LIST_TRUTH")
         (file->string (getenv "TEST_LIST_TRUTH"))))

      (define prec (hash-ref result 'precision))
      (define rec  (hash-ref result 'recall))

      (with-check-info
        (('prec prec)
         ('rec  rec))
        (check-true (> prec 0) "Nonzero precision")
        (check-true (> rec  0) "Nonzero recall")))))

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
