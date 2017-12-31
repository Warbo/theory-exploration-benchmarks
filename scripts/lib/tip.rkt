#lang racket

;; Helpers for manipulating TIP benchmarks

(require lib/compare)
(require lib/impure)
(require lib/lists)
(require lib/memo)
(require lib/util)

(provide definition? expression-constructors expression-destructors
         files-to-hashes get-def-s lowercase-names names-in
         native-symbols path-end symbols-in theorem? theorem-files
         theorem-hashes theorem-ids tip-benchmarks? tip-path?
         toplevel-function-defs-of toplevel-function-names-in toplevel-names-in
         uppercase-names)

(module+ test
  (require lib/testing))

;; Predicates, types and contracts

;; A TIP definition, e.g. for a type or function
(define (definition? x)
  (and (list? x)
       (not (empty? x))
       (member (first x) '(declare-datatypes
                           define-fun
                           define-fun-rec
                           define-funs-rec))))

;; A TIP theorem statement
(define (theorem? x)
  (match x
    [(cons 'assert-not _) #t]
    [_                    #f]))

;; A TIP benchmark problem, i.e. a list of definitions and a theorem statement
(define (tip-problem? x)
  (match x
    [(list defs thm) (and (list? defs)
                          (not (empty? defs))  ;; Overkill, but helps spot bugs
                          (all-of definition? defs)
                          (theorem? thm))]
    [_ #f]))

(module+ test
  (def-test-case "TIP problem accepts"
    (check-true (tip-problem? (list (list nat-def)
                                    '(assert-not (= 1 1))))))
  (def-test-case "TIP problem rejects"
    (check-false (tip-problem? (list nat-def)))
    (check-false (tip-problem? (list (list nat-def) nat-def)))))

;; The filename of a TIP problem, used to disambiguate type/function names. To
;; avoid ambiguous filenames, we include their directory, but we don't go any
;; further up the path, because we don't want such system-specific details to
;; affect things like our sampling.
(define (tip-path? x)
  (and (string? x)
       (string-suffix? x ".smt2")
       (string-contains? x "/")
       (= (length (string-split x "/")) 2)
       (not (string-prefix? x "/"))
       (not (string-suffix? x "/.smt2"))))

(module+ test
  (def-test-case "Path predicate accepts"
    (for-each (lambda (pth)
                (with-check-info
                  (('path pth))
                  (check-true (tip-path? pth))))
              '("grammars/packrat_unambigPackrat.smt2"
                "tip2015/propositional_AndCommutative.smt2"
                "tip2015/propositional_Sound.smt2"
                "tip2015/propositional_Okay.smt2"
                "tip2015/regexp_RecSeq.smt2"
                "tip2015/relaxedprefix_correct.smt2"
                "tip2015/propositional_AndIdempotent.smt2"
                "tip2015/propositional_AndImplication.smt2")))
  (def-test-case "Path predicate rejects"
    (for-each (lambda (pth)
                (with-check-info
                  (('path pth))
                  (check-false (tip-path? pth))))
              '(;; No .smt2 extension
                "foo"
                "grammars/simp_expr_unambig1"

                ;; No directory
                "propositional_AndCommutative.smt2"
                "simp_expr_unambig4.smt2"

                ;; Empty names
                "tip2015/.smt2"
                "/regexp_RecSeq.smt2"

                ;; Too much path
                "benchmarks/grammars/packrat_unambigPackrat.smt2"
                "/nix/store/1vgm16yp1vrwn8a9mnn1ji62a0jgi3sl-tip-benchmarks/isaplanner/prop_10.smt2"))))

;; A set of TIP benchmark problems, mapping filenames to problems
(define (tip-benchmarks? x)
  (hash/c tip-path? tip-problem?))

;; Functions

;; Returns a list of names for any functions which the given expression defines.
;; Note that expr must itself be a definition; we don't look through its
;; subexpressions.
(define (toplevel-function-names-in expr)
  (match expr
    [(list 'define-fun     name _ _ _)                      (list name)]
    [(list 'define-fun     (list 'par _ (list name _ _ _))) (list name)]
    [(list 'define-fun-rec name _ _ _)                      (list name)]
    [(list 'define-fun-rec (list 'par _ (list name _ _ _))) (list name)]
    [(list 'define-funs-rec decs _) (map (lambda (dec)
                                           (if (equal? (first dec) 'par)
                                               (first (third dec))
                                               (first dec)))
                                         decs)]
    [_ null]))

(module+ test
  (def-test-case "Can get function names"
    (check-equal? (map toplevel-function-names-in `(,nat-def
                                                    ,constructorZ
                                                    ,constructorS))
                  '(() (constructor-Z) (constructor-S)))))

;; Returns all global names defined in expr. Note that this a) does not look for
;; definitions in sub-expressions of expr, and b) returns the names in a
;; deterministic order based on where they appear in the definition (e.g. if
;; there are mutually-recursive functions, or types with constructors and
;; destructors, etc.)
(define (toplevel-names-in expr)
  (append (toplevel-function-names-in expr)
          (match expr
             [(list 'declare-datatypes _ decs)
              (foldl (lambda (dec result)
                       (define type-name
                         (car dec))

                       (define constructor-decs
                         (cdr dec))

                       (define destructor-decs
                         (append-map cdr constructor-decs))

                       (append (list type-name)
                               (map first constructor-decs)
                               (map first destructor-decs)
                               result))
                     null
                     decs)]
             [_ null])))

;; Return the names of all functions defined in the given expr, including
;; destructors; i.e. those things which need lowercase initials in Haskell.
(define (lowercase-names expr)
  (define (go expr)
    (cons (toplevel-function-names-in expr)
          (match expr
            [(list 'declare-datatypes _ decs)
             (foldl (lambda (dec result)
                      (define constructor-decs
                        (cdr dec))
                      (define destructor-decs
                        (append-map cdr constructor-decs))
                      (cons (map first destructor-decs) result))
                    null
                    decs)]
            [(cons a b) (cons (go a) (go b))]
            [_          null])))

  (flatten (go expr)))

(module+ test
  (def-test-case "Lowercase name extraction"
    (check-equal?
     (list->set (lowercase-names
                 (file->list (benchmark-file
                              "tip2015/sort_NStoogeSort2Permutes.smt2"))))
     (list->set '(custom-p custom-succ custom-pred custom-ite custom-not
                           custom-nat-> custom-> custom-<= custom-or
                           custom-bool-converter custom-and head tail first second p
                           twoThirds third take sort2 null length elem drop splitAt
                           delete isPermutation append nstooge2sort2 nstoogesort2
                           nstooge2sort1)))

    (check-equal?
     (uppercase-names
      (file->list (benchmark-file "tip2015/sort_NStoogeSort2Permutes.smt2")))
     '(CustomNat CustomZ CustomS CustomInt CustomNeg CustomZero CustomPos
                 CustomBool CustomTrue CustomFalse list nil cons Pair Pair2 Nat Z S))))

;; Return the names of all types and constructors defined in the given expr;
;; i.e. those things which need uppercase initials in Haskell.
(define (uppercase-names expr)
  (match expr
    [(list 'declare-datatypes _ type-decs)
     (append
      ;; Type names
      (map first type-decs)
      ;; Constructor names
      (append-map (lambda (type-dec)
                    (define constructor-decs
                      (cdr type-dec))
                    (map first constructor-decs))
                  type-decs))]

    [(cons a b) (append (uppercase-names a) (uppercase-names b))]
    [_          null]))

;; Returns all names defined in DEFS
(define (names-in defs)
  (append (lowercase-names defs) (uppercase-names defs)))

(module+ test
  (define (names-match src expr expect)
    (define names (names-in expr))

    (with-check-info
      (('src     src)
       ('expr    expr)
       ('expect  expect)
       ('names   names)
       ('message "Got expected names"))
      (check-equal? (list->set names) (list->set expect))))

  (def-test-case "Can find names defined in expressions"
      (names-match "datatype"
               '(declare-datatypes (a) ((list (nil)
                                              (cons (head a) (tail (list a))))))
               '(list nil cons head tail))

  (names-match "function"
               '(define-fun sort2
                  ((x Int) (y Int)) (list Int)
                  (ite (<= x y)
                       (cons x (cons y (as nil (list Int))))
                       (cons y (cons x (as nil (list Int))))))
               '(sort2))

  (names-match "parameterised function"
               '(define-fun
                  (par (a)
                       (zsplitAt ((x Int) (y (list a))) (Pair (list a) (list a))
                                 (Pair2 (ztake x y) (zdrop x y)))))
               '(zsplitAt))

  (names-match "recursive function"
               '(define-fun-rec insert2
                  ((x Int) (y (list Int))) (list Int)
                  (match y
                    (case nil (cons x (as nil (list Int))))
                    (case (cons z xs) (ite (<= x z)
                                           (cons x y)
                                           (cons z (insert2 x xs))))))
               '(insert2))

  (names-match "recursive parameterised function"
               '(define-fun-rec
                  (par (a)
                       (ztake ((x Int) (y (list a))) (list a)
                              (ite (= x 0)
                                   (as nil (list a))
                                   (match y
                                     (case nil (as nil (list a)))
                                     (case (cons z xs) (cons z
                                                             (ztake (- x 1)
                                                                    xs))))))))
               '(ztake))

  (names-match "mutually recursive functions"
               '(define-funs-rec
                  ((stooge2sort2 ((x (list Int))) (list Int))
                   (stoogesort2  ((x (list Int))) (list Int))
                   (stooge2sort1 ((x (list Int))) (list Int)))
                  ((match (zsplitAt (div (+ (* 2 (zlength x)) 1) 3) x)
                     (case (Pair2 ys zs) (append (stoogesort2 ys) zs)))
                   (match x
                     (case nil (as nil (list Int)))
                     (case (cons y z)
                       (match z
                         (case nil (cons y (as nil (list Int))))
                         (case (cons y2 x2)
                           (match x2
                             (case nil (sort2 y y2))
                             (case (cons x3 x4)
                               (stooge2sort2 (stooge2sort1 (stooge2sort2 x)))))))))
                   (match (zsplitAt (div (zlength x) 3) x)
                     (case (Pair2 ys zs) (append ys (stoogesort2 zs))))))
               '(stooge2sort2 stoogesort2 stooge2sort1)))

    (def-test-case "Symbol lookup"
    (define (symbols-from-file f)
      (names-in (file->list f)))

    (define (should-have syms kind xs)
      (for-each (lambda (sym)
                  (with-check-info
                   (('sym  sym)
                    ('syms syms)
                    ('kind kind))
                   (check-true (set-member? syms sym))))
                xs))

    (define (should-not-have syms kind xs)
      (for-each (lambda (sym)
                  (with-check-info
                   (('sym  sym)
                    ('syms syms)
                    ('kind kind))
                   (check-false (set-member? syms sym))))
                xs))

    (let* ([f    (benchmark-file "isaplanner/prop_84.smt2")]
           [syms (symbols-from-file f)])

      (should-have syms 'constructor '(CustomTrue CustomFalse nil cons Pair2 Z
                                                  S))

      (should-have syms 'destructor  '(head tail first second p))

      (should-have syms 'function '(custom-bool-converter zip take len drop
                                                          append))

      (should-not-have syms 'variable '(a b x y z x2 x3 x4 xs ys zs m m2 n n2 n3
                                          o))

      (should-not-have syms 'keyword  '(match
                                        case
                                        define-fun
                                        define-funs-rec
                                        declare-datatypes
                                        assert-not
                                        forall
                                        =
                                        check-sat)))

    (let* ([f    (benchmark-file "tip2015/list_PairEvens.smt2")]
           [syms (symbols-from-file f)])
      (should-not-have syms 'higher-order-type '(=>)))

    (let* ([f    (benchmark-file "tip2015/propositional_AndCommutative.smt2")]
           [syms (symbols-from-file f)])
      (should-have syms 'function '(or2)))))

;; The last part of a path, which is enough to distinguish a TIP benchmark
(define (path-end s)
  (define bits (string-split s "/"))

  (if (< (length bits) 2)
      (error (string-append "Given path '" s "' doesn't have 2 /-separated "
                            "components. We require this to a) avoid ambiguity "
                            "when combining files with the same name and b) to "
                            "prevent system-dependent absolute paths polluting "
                            "our output, affecting our sampling, etc."))
      (string-join (take-from-end 2 bits) "/")))


(define (split-contents exprs)
  (define thm #f)
  (define defs (filter (lambda (expr)
                         (match expr
                           [(cons 'assert-not _) (let ()
                                                   (set! thm expr)
                                                   #f)]
                           [(cons 'check-sat _)  #f]
                           [_                    #t]))
                       exprs))
  (list defs thm))

(define/test-contract (files-to-hashes files)
  (-> (*list/c string?) tip-benchmarks?)
  (foldl (lambda (f result)
           (hash-set result
                     (path-end f)
                     (split-contents (read-benchmark (file->string f)))))
         (hash)
         files))

;; Return any definitions of function F which appear in X
(define (toplevel-function-defs-of f x)
  (define (match-expr e)
    (match e
      [(list 'define-fun name _ _ _)                          (if (ss-eq? name f)
                                                                  (list e)
                                                                  #f)]
      [(list 'define-fun (list 'par _ (list name _ _ _)))     (if (ss-eq? name f)
                                                                  (list e)
                                                                  #f)]
      [(list 'define-fun-rec   name _ _ _)                    (if (ss-eq? name f)
                                                                  (list e)
                                                                  #f)]
      [(list 'define-fun-rec (list 'par _ (list name _ _ _))) (if (ss-eq? name f)
                                                                  (list e)
                                                                  #f)]
      [(list 'define-funs-rec names _)  (if (any-of (lambda (n)
                                                      (match n
                                                        [(list 'par ps (list name args return))
                                                         (ss-eq? name f)]
                                                        [(list name args return)
                                                         (ss-eq? name f)]))
                                                    names)
                                            (list e)
                                            #f)]
      [(cons 'declare-datatypes _) #f]
      [(cons 'assert-not _)        #f]
      [_                           #t]))
  (let ([found (match-expr x)])  ;; see if x is a definition of f
    (match found
      ;; No, x defines something else.
      [#f '()]

      ;; No, but x might contain f's definition.
      [#t (foldl (lambda (elem result)
                   (match (match-expr elem)
                     [#f          result]
                     [#t          result]
                     [(cons d _) (cons d result)]))
                 '()
                 x)]

      ;; Yes. Return it as-is.
      [_   found])))

(module+ test
  (def-test-case "Can find toplevel function definitions"
    (check-equal? (toplevel-function-defs-of 'constructor-Z
                                             `(,nat-def
                                               ,constructorZ
                                               ,constructorS))
                  (list constructorZ))))


;; Return any definitions of NAME appearing in EXPRS, where NAME can be of a
;; function, type, constructor or destructor
(define/test-contract (get-def-s name exprs)
  (-> symbol? any/c (*list/c any/c))

  (define/test-contract (defs-from exp)
    (-> any/c any/c)

    (match exp
      [(list 'declare-datatypes _ decs) (if (member name (names-in exp))
                                            (list exp)
                                            '())]
      [(cons a b)                       (let ([in-a (defs-from a)])
                                          (if (empty? in-a)
                                              (defs-from b)
                                              in-a))]
      [_                                null]))

  (remove-duplicates (append (toplevel-function-defs-of name exprs)
                             (defs-from exprs))))

;; Keywords of the TIP format. We specifically *avoid* parts of the TIP
;; specification like Int, Bool and their associated operations, which should be
;; replaced by the strip-native.rkt script. Due to constraints of the TIP format
;; some of these are currently unavoidable, e.g. polymorphic = which produces a
;; Bool; they shouldn't appear outside our wrappers though. Also, => should only
;; be used for function types, use custom-=> for boolean implication.
(define native-symbols
  '(ite Bool = distinct => ;; Should only appear as per strip-native.rkt
        @ as forall assert-not lambda case match let default
          declare-datatypes define-fun define-fun-rec define-funs-rec par))

;; Given an arbitrary TIP (sub)expression, return the externally-visible symbols
;; it contains. This includes globals being defined, globals being used,
;; functions, types and other values, but does *not* include keywords or bound
;; local variables (e.g. using 'let or 'lambda)
(define (symbols-in exp)
  (define (case-symbols c)
    (match c
      ;; Remove the symbols occuring in pat from body. This will remove fresh
      ;; variables, but may also remove constructors. That's fine though,
      ;; since we extract constructors separately anyway.
      ;; Note that "default" will be removed anyway, since it's a native symbol
      [(list 'case pat body)      (remove* (flatten (if (list? pat)
                                                        (cdr pat)
                                                        '()))
                                           (cons (if (list? pat)
                                                     (car pat)
                                                     pat)
                                                 (flatten (go body))))]
      [_                          (error "Unexpected case form")]))

  (define (go exp)
    (match exp
      [(cons 'match (cons arg cases)) (cons (go arg)
                                            (go (map case-symbols cases)))]
      [(list 'lambda args body)       (remove* (map car args)
                                               (flatten (go body)))]
      [(list 'let defs body)          (remove* (map car defs)
                                               (flatten
                                                (cons (go (map cdr defs))
                                                      (go body))))]
      [(list 'as val typ)             (cons (go val) (go typ))]
      [(list 'forall args body)       (remove* (map first args)
                                               (flatten
                                                (go (cons (map second args)
                                                          body))))]
      [(list 'assert-not body)        (go body)]
      [(list 'par args body)          (remove* args (go body))]
      [(cons a b)                     (cons (go a) (go b))]
      [_                              (if (symbol? exp) (list exp) null)]))

  (remove* native-symbols (flatten (go exp))))

(module+ test
  (def-test-case "Symbols in expression"
    (check-equal?
     (list->set (symbols-in '(match foo
                               (case bar      baz)
                               (case (quux x) (foo x baz)))))
     (set 'foo 'bar 'baz 'quux)
     "Pattern matches")

    (check-equal?
     (list->set (symbols-in '(match foo
                               (case default bar)
                               (case baz     quux))))
     (set 'foo 'bar 'baz 'quux)
     "Default case"))

  (def-test-case "Symbols in theorems"
    (check-equal? (set 'Foo 'bar 'baz)
                  (list->set (symbols-in
                              '(assert-not (forall ((x Foo) (y Foo))
                                                   (= (bar x y) (bar baz x))))))
                  "Found symbols in theorem")
    (check-equal?
     (set 'foo/bar.smt2CustomBool 'foo/bar.smt2list
          'foo/bar.smt2custom-bool-converter 'foo/bar.smt2append
          'foo/bar.smt2takeWhile 'foo/bar.smt2dropWhile)
     (list->set (symbols-in
                 '(assert-not
                   (par (local-a)
                        (forall ((local-p (=> local-a foo/bar.smt2CustomBool))
                                 (local-xs (foo/bar.smt2list local-a)))
                                (foo/bar.smt2custom-bool-converter
                                 (= (foo/bar.smt2append
                                     (foo/bar.smt2takeWhile local-p local-xs)
                                     (foo/bar.smt2dropWhile local-p local-xs))
                                    local-xs)))))))
     "Found symbols in qualified, parameterised theorem")))

;; Return a list of constructors defined in a given expression, e.g. '(Nil Cons)
;; if given a definition of List
(define (expression-constructors exp)
  (define (constructor-symbols c)
    (match c
      [(cons name vars) (list name)]
      [_                (error "Unexpected constructor form")]))

  (define (constructors-from-def decs)
    (foldl (lambda (dec got)
             (append got (match dec
                           [(cons type defs) (map first defs)])))
           '()
           decs))
  (match exp
    [(list 'declare-datatypes given decs) (constructors-from-def decs)]
    [(cons a b)                           (append (expression-constructors a)
                                                  (expression-constructors b))]
    [_                                    null]))

;; Return a list of destructors defined in EXP
(define (expression-destructors exp)
    (define (destructor-symbols c)
      (match c
        [(cons name vars) (map car vars)]
        [_                (error "Unexpected destructor form")]))

    (define (destructors-from-def decs)
      (symbols-in (foldl (lambda (dec got)
                           (append got (match dec
                                         [(cons type defs) (symbols-in (map destructor-symbols defs))]
                                         [_                (error "Unexpected type def")])))
                         null
                         decs)))
    (match exp
      [(list 'declare-datatypes given decs) (destructors-from-def decs)]
      [(cons a b)                           (append (expression-destructors a)
                                                    (expression-destructors b))]
      [_                                    null]))

(define expression-funs
  (let ()
    (define (fun-rec-expressions decs defs)
      (match (list decs defs)
        [(list (cons (list 'par ps (list name args return)) more-decs)
               (cons body                                   more-defs))
         (cons (cons name (remove* (append ps (map car args))
                                   (symbols-in body)))
               (fun-rec-expressions more-decs more-defs))]

        [(list (cons (list name args return) more-decs)
               (cons body                    more-defs))
         (append (cons name (remove* (map car args)
                                     (symbols-in body)))
                 (fun-rec-expressions more-decs more-defs))]

        [_ null]))

    (define (go exp)
      (match exp
        [(list 'define-fun-rec
               (list 'par p
                     (list name args return body)))
         (cons name (remove* (append (map car args)
                                     (symbols-in p))
                             (symbols-in body)))]

        [(list 'define-fun-rec name args return body)
         (cons name (remove* (map car args) (symbols-in body)))]

        [(list 'define-funs-rec decs defs)
         (fun-rec-expressions decs defs)]

        [(list 'define-fun
               (list 'par p
                     (list name args return body)))
         (cons name (remove* (append (map car args) (symbols-in p))
                             (symbols-in body)))]

        [(list 'define-fun     name args return body)
         (cons name (remove* (map car args) (symbols-in body)))]

        [(cons a b)                                   (cons (expression-funs a)
                                                            (expression-funs b))]

        [_                                            null]))

    (memo1 (lambda (exp) (flatten (go exp))))))

(memo0 theorem-hashes (files-to-hashes (theorem-files)))

(memo0 theorem-ids (map path-end (theorem-files)))

(module+ test
  (def-test-case "Can find constructor wrappers"
    (check-equal? (get-def-s 'constructor-Z redundancies)
                  (list constructorZ))))

(module+ test
  (def-test-case "List manipulation"
    (check-equal? (symbols-in '(lambda ((local1 Nat) (local2 (List Nat)))
                                 (free1 local1)))
                  '(free1)))

  (check-equal? (names-in '(fee fi fo fum)) null)
  (check-equal? (names-in '(define-funs-rec
                             ((stooge1sort2 ((x (list Int))) (list Int))
                              (stoogesort ((x (list Int))) (list Int))
                              (stooge1sort1 ((x (list Int))) (list Int)))
                             ((match (zsplitAt (div (zlength x) 3) (reverse x))
                                (case (Pair2 ys zs) (append (stoogesort zs) (reverse ys))))
                              (match x
                                (case nil (as nil (list Int)))
                                (case (cons y z)
                                  (match z
                                    (case nil (cons y (as nil (list Int))))
                                    (case (cons y2 x2)
                                      (match x2
                                        (case nil (sort2 y y2))
                                        (case (cons x3 x4)
                                          (stooge1sort2 (stooge1sort1 (stooge1sort2 x)))))))))
                              (match (zsplitAt (div (zlength x) 3) x)
                                (case (Pair2 ys zs) (append ys (stoogesort zs)))))))
                '(stooge1sort2 stoogesort stooge1sort1))


    (def-test-case "Can get form constructors"
      (check-equal? (get-def-s '& form-with-deps)
                    (list form))))
