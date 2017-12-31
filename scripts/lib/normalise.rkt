#lang racket

;; Normalise TIP definitions

(require racket/trace)
(require lib/compare)
(require lib/impure)
(require lib/lists)
(require lib/memo)
(require lib/replacements)
(require lib/tip)
(require lib/util)
(require lib/strip-native)

(provide all-constructor-function-replacements all-replacements-closure
         decode-name
         decode-string encode-lower-name final-benchmark-defs
         gen-normed-and-replacements
         lowercase-benchmark-names mk-final-defs
         mk-final-defs-hash nn norm-name
         normed-and-replacements-cached normed-qualified-theorem-files prepare
         qual-hashes-theorem-files
         replace-names unqualify)

(module+ test
  (require lib/testing))

;; Unencoded names. Strictly speaking, we should allow such names in case the
;; user actually fed in such definitions; however, in practice this is a good
;; indicator that something's wrong in our logic!
(define (unencoded? exprs)
  (all-of (lambda (name)
            (not (regexp-match? "[Gg]lobal[0-9a-f]+"
                                (symbol->string name))))
          (names-in exprs)))

;; Like unencoded?, but make sure we're not given normalised names
(define (unnormalised? exprs)
  (all-of (lambda (name)
            (not (regexp-match? "normalise-var-[0-9]"
                                (symbol->string name))))
          (names-in exprs)))

(define (is-custom? name)
  (define str (string-downcase (symbol->string name)))
  (or (string-contains? str ".smt2custom")
      (string-prefix?   str "custom")))

;; Normalise an expression: all type parameters, local variables, global
;; definitions, etc. are replaced with sequential names. References to global
;; names are left intact. This allows easy alpha-equivalence checking: A and B
;; are alpha-equivalent iff (equal? (norm A) (norm B))
(define/test-contract (norm expr)
  (-> (and/c definition?
             ;; Our input should have locals prefixed
             (lambda (input)
               (all-of (lambda (name)
                         (or (regexp-match? "^local-[0-9]*$"
                                            (symbol->string name))
                             (raise-user-error
                              'norm
                              "Local '~a' hasn't been prefixed in:\n~a\n"
                              name
                              input)))
                       (locals-in input))))
      (and/c definition?
             ;; Our result should be normalised
             (lambda (result)
               (all-of (lambda (name)
                         (or (regexp-match? "^defining-name-[0-9]*$"
                                            (symbol->string name))
                             (regexp-match? "^local-[0-9]*$"
                                            (symbol->string name))
                             (raise-user-error
                              'norm
                              "Name '~a' wasn't normalised away in:\n~a\n"
                              name
                              result)))
                       (names-in result)))))
  (norm-names-in (toplevel-names-in expr) expr))

(define (norm-names-in names expr)
  (define new (map (lambda (n)
                     (string->symbol
                      (string-append "defining-name-" (~a (+ 1 n)))))
                   (range (length names))))

  (replace-all (zip names new) expr))

(module+ test
  (def-test-case "Norm"
    (check-equal? '(declare-datatypes
                    ()
                    ((defining-name-1
                       (defining-name-2)
                       (defining-name-3
                        (defining-name-4 defining-name-1)))))
                  (norm nat-def))))

;; Looks through EXPR for the highest sequential name, and returns the next name
;; in the sequence
(define (next-var expr)
  (define (var-num x)
    (string->number (substring (symbol->string x) (string-length var-prefix))))

  (define (max-var expr)
    (if (and (symbol? expr)
             (string-prefix? (symbol->string expr) var-prefix))
        expr
        (match expr
          [(cons a b) (let ([ma (max-var a)]
                            [mb (max-var b)])
                        (if (< (var-num ma) (var-num mb))
                            mb
                            ma))]
          [_          (string->symbol (string-append var-prefix "0"))])))

  (string->symbol
   (string-append var-prefix
                  (number->string (+ 1 (var-num (max-var expr)))))))

;; Returns a name beginning with PRE and ending with one more than N
(define (inc-name pre n)
  (string->symbol (string-append pre (number->string (+ 1 n)))))

;; Prefix used when normalising expressions
(define         var-prefix "normalise-var-")

;; Returns the highest sequential name in EXPR with the given prefix
(define (max-name pre expr)
  (define (name-num pre x)
    (if (symbol? x)
        (if (string-prefix? (symbol->string x) pre)
            (string->number (substring (symbol->string x)
                                       (string-length pre)))
            0)
        0))

  (match expr
    [(cons a b) (let ([ma (max-name pre a)]
                      [mb (max-name pre b)])
                  (if (< ma mb) mb ma))]
    [x          (name-num pre x)]))

;; Theorems are anonymous, so we use their path as a name, since it's stable
;; even when we rewrite the contents (e.g. for normalising)
(define (qualified-theorems? thms)
  (and/c (hash/c tip-path? theorem?)))

(define (prefixed-locals? x)
  (and  (definition? x)
        (or (equal? x (prefix-locals x))
            (raise-user-error
             'prefixed-locals?
             "Unprefixed locals in definition:\n~a\n"
             x))))

(define (add-custom-defs x)
  (append x
          (map (match-lambda
                 [(list def deps)
                  (prefix-locals def)])
               (list custom-bool custom-ite custom-not custom-and custom-or
                     custom-=> custom-bool-converter custom-nat custom-int
                     custom-plus custom-inc custom-dec custom-invert custom-abs
                     custom-sign custom-+ custom-- custom-* custom-nat->
                     custom-> custom-div custom-mod custom-< custom->=
                     custom-<=))))

;; Prefix each name with the path of the file it came from, and combine all
;; definitions together into one long list (ordered lexicographically by path).
;; Theorems are also qualified, but remain independent and keyed by their path.
(define/test-contract (qual-all-hashes given-hashes)
  (-> tip-benchmarks? (list/c (*list/c (and/c definition?
                                              prefixed-locals?))
                              qualified-theorems?))
  (define entries
    (sort (hash->list given-hashes) string>? #:key car))

  (define result
    (foldl (match-lambda*
             [(list (cons pth (list defs thm))
                    (list all thms))

              (list (cons (map (curry qualify pth) defs) all)
                    (hash-set thms pth (qualify pth thm)))])
           (list '() (hash))
           entries))

  (match result
    [(list defs thms) (list (append* defs) thms)]))

(module+ test
  (def-test-case "Can qualify filename/content hashes"
    (check-equal? (qual-all-hashes (hash "C/D.smt2"
                                         `((,nat-def)
                                           (assert-not (= foo bar)))

                                         "Y/Z.smt2"
                                         '(((declare-datatypes ()
                                              ((Nat (Z) (S (p Nat)))))
                                            (define-fun Z2 () Nat
                                              (as Z Nat))
                                            (define-fun S2 ((x Nat)) Nat
                                              (as (S x) Nat))
                                            (define-fun Z3 () Nat
                                              (as Z Nat))
                                            (define-fun Z4 () Nat
                                              (as Z Nat))
                                            (define-fun Z5 () Nat
                                              (as Z Nat)))
                                           (assert-not
                                            (forall ((x Nat))
                                                    (= (foo x) baz))))))
                  `(((declare-datatypes ()
                       ((C/D.smt2Nat
                         (C/D.smt2Z)
                         (C/D.smt2S
                          (C/D.smt2p C/D.smt2Nat)))))
                     (declare-datatypes ()
                       ((Y/Z.smt2Nat
                         (Y/Z.smt2Z)
                         (Y/Z.smt2S
                          (Y/Z.smt2p
                           Y/Z.smt2Nat)))))
                     (define-fun Y/Z.smt2Z2 ()
                       Y/Z.smt2Nat
                       (as Y/Z.smt2Z Y/Z.smt2Nat))
                     (define-fun Y/Z.smt2S2
                       ((local-1 Y/Z.smt2Nat))
                       Y/Z.smt2Nat
                       (as (Y/Z.smt2S local-1) Y/Z.smt2Nat))
                     (define-fun Y/Z.smt2Z3 ()
                       Y/Z.smt2Nat
                       (as Y/Z.smt2Z Y/Z.smt2Nat))
                     (define-fun Y/Z.smt2Z4 ()
                       Y/Z.smt2Nat
                       (as Y/Z.smt2Z Y/Z.smt2Nat))
                     (define-fun Y/Z.smt2Z5 ()
                       Y/Z.smt2Nat
                       (as Y/Z.smt2Z Y/Z.smt2Nat)))
                    ,(hash "C/D.smt2" '(assert-not (= C/D.smt2foo C/D.smt2bar))
                           "Y/Z.smt2" '(assert-not
                                        (forall ((local-1 Y/Z.smt2Nat))
                                                (= (Y/Z.smt2foo local-1)
                                                   Y/Z.smt2baz))))))))

;; Prefix all definitions in EXPR with NAME, and prefix all local variables
(define (qualify name expr)
  (foldl (lambda (sym x)
           (replace-in sym
                       (string->symbol (string-append name
                                                      (symbol->string sym)))
                       x))
         (prefix-locals expr)
         (symbols-in expr)))

(module+ test
  (def-test-case "Can qualify expressions"
    (define match-expr
      '(match foo
         (case bar      baz)
         (case (quux x) (foobar x foo))))

    (check-equal? '(match a/b.smt2foo
                     (case  a/b.smt2bar            a/b.smt2baz)
                     (case (a/b.smt2quux local-1) (a/b.smt2foobar local-1
                                                                  a/b.smt2foo)))
                  (qualify "a/b.smt2" match-expr)
                  "Pattern matching"))

  (def-test-case "Can qualify theorems"
    (check-equal? '(assert-not (forall
                                ((local-1 A/B.smt2Foo))
                                (= (A/B.smt2foo local-1)
                                   A/B.smt2bar)))
                  (qualify "A/B.smt2" '(assert-not (forall
                                                    ((x Foo))
                                                    (= (foo x) bar)))))))

;; Prefixes a local variable name
(define (prefix-local s)
  (string->symbol (string-append "local-" (as-str s))))

;; The local names introduced in an expression, e.g. arguments of a lambda.
(define (locals-in expr)
    (match expr
      [(list 'define-fun-rec (list 'par p (list name args return body)))
       (symbols-in (append p (map first args) (locals-in body)))]

      [(list 'define-fun-rec                    name args return body)
       (symbols-in (append   (map first args) (locals-in body)))]

      [(list 'define-fun     (list 'par p (list name args return body)))
       (symbols-in (append p (map first args) (locals-in body)))]

      [(list 'define-fun                        name args return body)
       (symbols-in (append   (map first args) (locals-in body)))]


      [(list 'define-funs-rec decs defs)
       (begin
         (define (from-dec dec)
           (define par  (equal? 'par (first dec)))
           (define pars (if par
                            (second dec)
                            '()))
           (define args (if par
                            (second (third dec))
                            (second dec)))
           (append pars (map first args)))

         (append (append-map from-dec decs)
                 (append-map locals-in defs)))]

      [(list 'declare-datatypes given decs)
       (symbols-in given)]

      [(list 'case pat body)
       (symbols-in (append (locals-in body)
                           (match pat
                             [(list con)    null]
                             [(cons con ps) ps]
                             [_             null])))]

      [(list 'lambda args body)
       (symbols-in (append (map first args) (locals-in body)))]

      [(list 'let bindings body)
       (symbols-in (append (map first bindings) (locals-in body)))]

      [(list 'assert-not body)
       (locals-in body)]

      [(list 'forall args body)
       (symbols-in (append (map first args) (locals-in body)))]

      [(list 'par args body)
       (append args (locals-in body))]

      [(cons a b) (append (locals-in a) (locals-in b))]

      [_ null]))

(module+ test
  (def-test-case "Can get locals"
    (check-equal? (set 'x)
                  (list->set (locals-in '(assert-not
                                          (forall ((x Nat)) (= x foo)))))
                  "Locals from theorems")
    (check-equal? (set 'a 'f)
                  (list->set (locals-in '(assert-not
                                          (par (a)
                                               (forall ((f (=> a Nat)))
                                                       (= 1 (f 2)))))))
                  "Parameterised theorems")))

;; Prefix all bound local variables appearing in EXPR, including arguments of
;; function definitions and lambda functions, let-bound variables and
;; pattern-match cases
(define (prefix-locals expr)
  ;; Turn bindings like (lambda (x Int) (f x)) into
  ;; (lambda (local-1 Int) (f local-1)) to prevent conflicts between local and
  ;; global names (e.g. if there's a destructor called x)

  ;; Note: expr may contain names like local-1, local-2, etc. already, since our
  ;; output may come back around at some point. To prevent clashes, we first
  ;; rename locals to prefixing-local-1, prefixing-local-2, etc. and then rename
  ;; these to local-1, local-2, etc. Hopefully this should avoid clashes, since
  ;; the prefixing-local-* names are never emitted in our output.
  (define (do-rename prefix expr)
    (foldl (lambda (local result)
             (match result
               [(list expr n)
                (list (replace-in local
                                  (string->symbol
                                   (string-append prefix (number->string n)))
                                  expr)
                      (+ 1 n))]))
           (list expr 1)
           (locals-in expr)))

  (first (do-rename "local-" (first (do-rename "prefixing-local-" expr)))))

(module+ test
  (def-test-case "Prefix locals"
    (define before
      '(define-fun-rec (par (a)
                            (length ((local-1 (list a)))
                                    nat
                                    (match local-1
                                      (case nil Z)
                                      (case (cons y z)
                                        (S (length z))))))))

    (define after
      '(define-fun-rec (par (local-1)
                            (length ((local-2 (list local-1)))
                                    nat
                                    (match local-2
                                      (case nil Z)
                                      (case (cons local-3 local-4)
                                        (S (length local-4))))))))

    (check-equal? after (prefix-locals before)
                  "Prefix locals of function definition")

    (check-equal? after (prefix-locals (prefix-locals before))
                  "Prefixing is idempotent")

    (check-true  (prefixed-locals? after))))

;; Removes qualification from our "custom" definitions, introduced by
;; strip-native.rkt, since they're not actually defined in any TIP file.
(define (unqualify x)
  ;; Removes any prefix from occurrences of the given name appearing in the
  ;; given expression
  (define (unqual name expr)
    (match expr
      [(? symbol?) (if (string-suffix? (symbol->string expr)
                                       (string-append ".smt2"
                                                      (symbol->string name)))
                       name
                       expr)]
      [(cons y ys) (cons (unqual name y) (unqual name ys))]
      [_           expr]))

  (foldl unqual
         x
         '(CustomBool CustomTrue CustomFalse
           custom-ite custom-not custom-and custom-or custom-=>
           custom-bool-converter
           CustomNat CustomZ CustomS custom-p custom-plus
           CustomInt CustomNeg custom-succ CustomZero CustomPos custom-pred
           custom-inc custom-dec custom-invert custom-abs custom-sign custom-+
           custom-- custom-* custom-nat-> custom-> custom-div custom-mod
           custom-< custom->= custom-<=)))

(module+ test
  (def-test-case "Can unqualify empty"
    (check-equal? (unqualify '()) '())))

;; Look through X for constructor definitions, and for each one append to X a
;; new function definition which simply wraps that constructor. For example, if
;; X defines a Nat type with constructors Z and S, we add new functions
;; constructor-Z and constructor-S which are eta-expansions of Z and S,
;; respectively.
(define (add-constructor-funcs x)
  (define (func-for c)
    ;; Look through x for a definition of c
    (define definition
      (first (get-def-s c x)))

    (define (arg-decs-for x)
      ;; Get the argument list of c
      (define (arg-decs-for-ty x)
        (define (arg-decs-for-con x)
          (match x
            [(list name)      (if (equal? name c) (list '())  '())]
            [(cons name args) (if (equal? name c) (list args) '())]))

        (append-map arg-decs-for-con (cdr x)))

      (match definition
        [(list 'declare-datatypes _ decs) (append-map arg-decs-for-ty decs)]))

    (define arg-decs
      (map (lambda (def)
             (cons (prefix-local (without-filename (car def)))
                   (cdr def)))
           (first (arg-decs-for x))))

    (define parameters
      (match definition
        [(list 'declare-datatypes ps _) ps]))

    (define (constructor-type x)
      (define (has-constructor dec)
        (member c (map first (cdr dec))))

      (match definition
        [(list 'declare-datatypes _ decs) (map first (filter has-constructor decs))]))

    (define name
      (prefix-name c "constructor-"))

    (define type
      (if (empty? parameters)
          (first (constructor-type x))
          (cons (first (constructor-type x)) parameters)))

    (define body
      (if (empty? arg-decs)
          `(as ,c ,type)
          `(as ,(cons c (map car arg-decs)) ,type)))

    (define func
      (if (empty? parameters)
          `(define-fun                   ,name ,arg-decs ,type ,body)
          `(define-fun (par ,parameters (,name ,arg-decs ,type ,body)))))

    (prefix-locals func))

  (append x (map func-for (expression-constructors x))))

(module+ test
  (def-test-case "Can add constructor functions"
    (check-equal? (add-constructor-funcs (list nat-def))
                  `(,nat-def ,(prefix-locals constructorZ)
                             ,(prefix-locals constructorS))))

  (def-test-case "Bare constructor function type"
    (check-equal? (add-constructor-funcs '((declare-datatypes
                                            ()
                                            ((MyBool (MyTrue) (MyFalse))))))
                  '((declare-datatypes
                     ()
                     ((MyBool (MyTrue) (MyFalse))))
                    (define-fun constructor-MyTrue  () MyBool
                      (as MyTrue  MyBool))
                    (define-fun constructor-MyFalse () MyBool
                      (as MyFalse MyBool)))))

  (def-test-case "Parameterised constructor function type"
    (check-equal? (add-constructor-funcs
                   '((declare-datatypes
                      (local-a)
                      ((MyStream (MyCons (myHead local-a)
                                         (myTail (MyStream local-a))))))))
                  '((declare-datatypes
                     (local-a)
                     ((MyStream (MyCons (myHead local-a)
                                        (myTail (MyStream local-a))))))
                    (define-fun
                      (par (local-1)
                           (constructor-MyCons
                            ((local-2 local-1)
                             (local-3 (MyStream local-1)))
                            (MyStream local-1)
                            (as (MyCons local-2 local-3)
                                (MyStream local-1)))))))))

;; Strips off any filename prefix from a symbol, in case it can't be written
;; verbatim as a symbol (TIP doesn't support escaping in symbol names like
;; Racket does, so this prevents Racket writing |foo| and TIP crashing)
(define (without-filename s)
  (string->symbol (last (string-split (symbol->string s) ".smt2"))))

;; For each destructor defined in X, appends a new function to X which is
;; equivalent to that destructor: i.e. it performs a pattern-match to return the
;; relevant argument of the relevant constructor. Note that these functions will
;; be partial for types with multiple constructors; for example, the "head" and
;; "tail" destructors for "list" will only match "cons" values.
(define (add-destructor-funcs x)
  (define (mk-destructor-func d)
    (define name
      (prefix-name d "destructor-"))

    (define (contains-d x)
      (member d (symbols-in x)))

    ;; The whole 'declare-datatypes expression
    (define whole-def
      (first (get-def-s d x)))

    (define parameters
      (match whole-def
        [(list 'declare-datatypes ps _) ps]))

    ;; The particular type declaration, e.g. if d is 'head we might get
    ;; '(List (Nil) (Cons (head a) (tail (List a))))
    (define type-def
      (match whole-def
        [(list 'declare-datatypes _ type-defs)
         (first (filter contains-d type-defs))]))

    ;; Not applied to any parameters
    (define type-name
      (first type-def))

    ;; Type applied to parameters, if any
    (define input-type
      (if (empty? parameters)
          type-name
          (cons type-name parameters)))

    ;; The particular constructor declaration, e.g. if d is 'head we might get
    ;; '(Cons (head a) (tail (List a)))
    (define con-def
      (first (filter contains-d (cdr type-def))))

    ;; The particular destructor declaration, e.g. if d is 'head we might get
    ;; '(head a)
    (define des-def
      (first (filter contains-d (cdr con-def))))

    (define output-type
      (second des-def))

    ;; Now we have enough info to build our eta-expanded function
    (define args
      `((destructor-arg ,input-type)))

    ;; For example, '(Cons local-head local-tail)
    (define pattern
      (cons (car con-def)
            (map (lambda (des)
                   (prefix-local (without-filename (car des))))
                 (cdr con-def))))

    ;; For example:
    ;;
    ;; '(match destructor-arg (case (Cons local-head local-tail) local-head))
    (define body
      `(match destructor-arg
         (case ,pattern ,(prefix-local (without-filename d)))))

    ;; For example:
    ;;
    ;; (define-fun (par (a)
    ;;   (destructor-head ((destructor-arg (List a))) a
    ;;     (match destructor-arg
    ;;       (case (Cons local-head local-tail) local-head)))))
    (define func
      (if (empty? parameters)
          `(define-fun       ,name ,args ,output-type ,body)
          `(define-fun (par ,parameters
                            (,name ,args ,output-type ,body)))))

    ;; Prefix any parameters which aren't already, to avoid clashing with global
    ;; names
    (define unprefixed-parameters
      (filter (lambda (parameter)
                (not (regexp-match? #rx"^local-" (symbol->string parameter))))
              parameters))

    (prefix-locals (replace-all (zip unprefixed-parameters
                                     (map prefix-local unprefixed-parameters))
                                func)))

  (foldl (lambda (d expr)
           (append expr (list (mk-destructor-func d))))
         x
         (expression-destructors x)))

(module+ test
  (def-test-case "Can add destructor functions for recursive types"
    (check-equal? (add-destructor-funcs '((declare-datatypes
                                           ()
                                           ((Nat (Z) (S (p Nat)))))))
                  '((declare-datatypes
                     ()
                     ((Nat (Z) (S (p Nat)))))
                    (define-fun destructor-p ((local-1 Nat)) Nat
                      (match local-1
                        (case (S local-2) local-2))))))

  (def-test-case "Can add destructor functions for parameterised types"
    (check-equal? (add-destructor-funcs
                   '((declare-datatypes
                      (a b)
                      ((OneAndMany (One (theOne a))
                                   (Many (head b)
                                         (tail (OneAndMany a b))))))))
                  '((declare-datatypes
                     (a b)
                     ((OneAndMany (One (theOne a))
                                  (Many (head b)
                                        (tail (OneAndMany a b))))))
                    (define-fun
                      (par (local-1 local-2)
                           (destructor-theOne
                            ((local-3 (OneAndMany local-1 local-2)))
                            local-1
                            (match local-3
                              (case (One local-4) local-4)))))
                    (define-fun
                      (par (local-1 local-2)
                           (destructor-head
                            ((local-3 (OneAndMany local-1 local-2)))
                            local-2
                            (match local-3
                              (case (Many local-4 local-5) local-4)))))
                    (define-fun
                      (par (local-1 local-2)
                           (destructor-tail
                            ((local-3 (OneAndMany local-1 local-2)))
                            (OneAndMany local-1 local-2)
                            (match local-3
                              (case (Many local-4 local-5)
                                local-5)))))))))

;; Decode an encoded "globalXXX" or "GlobalXXX" name
(define (decode-name name)
  (define no-global
    (substring (symbol->string name) 6))

  (string->symbol (decode16 no-global)))

;; Replace function names with "globalXXX" where "XXX" is a hex encoding of
;; the name, and replace type and constructor names with "GlobalXXX" where "XXX"
;; is a hex encoding of the name. These will survive translation back and forth
;; between TIP and Haskell.
(define (encode-names expr)
  (define lower-encoded
    (foldl (lambda (name e)
             (replace-in name (encode-lower-name name) e))
           expr
           (lowercase-names expr)))

  (foldl (lambda (name e)
           (replace-in name (encode-upper-name name) e))
         lower-encoded
         (uppercase-names expr)))

(module+ test
  (def-test-case "Can encode names"
    (check-equal? (encode-names (list nat-def))
                  '((declare-datatypes ()
                      ((Global4e6174
                         (Global5a)
                         (Global53 (global70 Global4e6174))))))
                  "Simple definition")

    ;; Adapted from tip2015/escape_Injective.smt2
    (check-equal? (encode-names
                   '((declare-datatypes () ((CustomBool (CustomTrue) (CustomFalse))))
                     (declare-datatypes () ((Token (A) (B) (C) (D) (ESC) (P) (Q) (R))))
                     (define-fun isSpecial
                       ((local-1 Token)) CustomBool
                       (match local-1
                         (case default                          CustomFalse)
                         (case ESC CustomTrue)
                         (case P   CustomTrue)
                         (case Q   CustomTrue)
                         (case R   CustomTrue)))))
                  '((declare-datatypes () ((Global437573746f6d426f6f6c
                                            (Global437573746f6d54727565)
                                            (Global437573746f6d46616c7365))))
                    (declare-datatypes () ((Global546f6b656e
                                            (Global41)
                                            (Global42)
                                            (Global43)
                                            (Global44)
                                            (Global455343)
                                            (Global50)
                                            (Global51)
                                            (Global52))))
                    (define-fun global69735370656369616c
                      ((local-1 Global546f6b656e))
                      Global437573746f6d426f6f6c
                      (match local-1
                        (case default      Global437573746f6d46616c7365)
                        (case Global455343 Global437573746f6d54727565)
                        (case Global50     Global437573746f6d54727565)
                        (case Global51     Global437573746f6d54727565)
                        (case Global52     Global437573746f6d54727565))))
                  "Match with default")))

;; Encode a function name for surviving Haskell translation
(define (encode-lower-name name)
  (string->symbol (string-append "global"
                                 (encode16 (symbol->string name)))))

;; Encode a type or constructor name for surviving Haskell translation
(define (encode-upper-name name)
  (string->symbol (string-append "Global"
                                 (encode16 (symbol->string name)))))

;; Replace occurences of "GlobalXXXX" and "globalXXXX" in S with their decoded
;; variants
(define (decode-string s)
  (first
   (foldl (lambda (pair s-diff)
            (define s
              (first s-diff))

            (define diff
              (second s-diff))

            (define start
              (+ diff (car pair)))

            (define end
              (+ diff (cdr pair)))

            (define prefix
              (substring s 0 start))

            (define suffix
              (substring s end))

            (define encoded-name
              (substring s (+ start (string-length "global")) end))

            (define name
              (decode16 encoded-name))

            (define new-diff
              (- end start (string-length name)))

            (list (string-append prefix name suffix) (- diff new-diff)))
          (list s 0)
          (regexp-match-positions* #rx"[Gg]lobal[0-9a-f]+" s))))

(module+ test
  (def-test-case "Decode strings"
    (check-equal? (decode-string "foo Global68656c6c6f bar global776f726c64")
                  "foo hello bar world")))

;; Turns a list of definitions X into a form suitable for sending into the tip
;; tools
(define (prepare x)
  (define (add-check-sat x)
    ;; Add '(check-sat)' as the last line to appease tip-tools
    (append x '((check-sat))))

  (add-check-sat
   (encode-names
    (preprepare x))))

(memo0 qual-hashes-theorem-files
  (qual-all-hashes (theorem-hashes)))

(module+ test
  (def-test-case "Have qual-hashes-theorem-files"
    (check-true (list? (qual-hashes-theorem-files)))))

(define (preprepare x)
  (add-constructor-funcs
   (add-destructor-funcs
    (unqualify x))))

(module+ test
  (def-test-case "Can 'preprepare' a list of definitions"
    (check-equal? '() (preprepare '()) "Empty")))

(define/test-contract (names-to-reps names)
  (-> (*list/c (and/c (non-empty-listof (non-empty-listof symbol?))
                      (lambda (equivs)
                        (all-of (compose (curry equal? (length (first equivs)))
                                         length)
                                equivs))))
      replacements?)
  ;; Combine all accumulated replacement sets
  (apply extend-replacements
         (foldl
          (lambda (equivs result)
            ;; equivs is '((n1 n2 ...) (m1 m2 ...) ...) where n1, m1, ... are
            ;; equivalent, n2, m2, ... are equivalent, and so on.

            (if (equal? 1 (length equivs))
                ;; Nothing to replace
                result

                ;; Loop once for each element in the lists (e.g. n1, n2, ...).
                ;; 'included' updates each iteration, and is returned as result.
                (for/fold ([included result])
                          ([_ (first equivs)] ;; stop when names run out
                           [n (in-naturals 0)]) ;; names to add this iteration

                  ;; Get nth element of each list in equivs
                  (let ([nths (map (curry nth n) equivs)])

                    ;; Turn into replacement set and accumulate
                    (cons (apply replacement nths) included)))))
          '()
          names)))

(module+ test
  (def-test-case "Replacement contracts match up"
    (for-each (compose check-true replacements? names-to-reps)
              '( (((A)))
                 (((A) (B))))))

  (def-test-case "Convert lists of names to replacement sets"
    (check-equal? '(A A (bar A C C (B bar bar C) A) bar)
                  (replace (finalise-replacements (names-to-reps
                                                   '(((A B C) (P Q R) (X Y Z))
                                                     ((foo) (bar)))))
                           '(A P (foo X Z Z (Q bar bar C) A) foo))
                  "Replacements get smallest value from lists")))

(define (normalised-intermediate? x)
  (list/c
   (*list/c names-def-pair?)
   (hash/c definition? (and/c (non-empty-listof (*list/c symbol?))
                              (lambda (namess)
                                ;; Each alternative list of names should be the
                                ;; same length
                                (define len (length (first namess)))
                                (all-of (lambda (names)
                                          (equal? (length names) len))
                                        namess))))))

;; A pair where the first is a list of names, and the second is a normalised
;; definition. Plugging the names into the definition recovers the original.
(define names-def-pair? (list/c (*list/c symbol?) definition?))

(define/test-contract (mk-output pair so-far)
  (-> names-def-pair?
      normalised-intermediate?
      normalised-intermediate?)
  (match pair
    [(list names norm-def)

     (match so-far
       [(list defs reps)

        (list
         (if (hash-has-key? reps norm-def)
             defs
             (cons pair defs))
         (hash-update reps norm-def
                      (curry cons names) ;; Prepend names to any existing list
                      (list names)))])])) ;; Use names as-is if no entry exists

(module+ test
  (let ([output (second
                 (mk-output (split-off-names constructorZ)
                            (list '() (hash (norm constructorZ)
                                            '((existing-Z))))))])
    (def-test-case "Redundant output contains normalised defs"
      (check-equal? (hash-keys output)
                    `(,(norm constructorZ))))

    (def-test-case "Redundant output contains classes of equivalent names"
      (check-equal? (hash-values output)
                    `(((constructor-Z) (existing-Z)))))

    (def-test-case "Redundancy output"
      (check-equal? output
                    (hash (norm constructorZ) '((constructor-Z) (existing-Z)))))))

;; Looks for alpha-equivalent definitions in RAW-EXPRS, and returns a set of
;; name replacements which can be used to update references and remove
;; redundancies.
(define/test-contract (find-redundancies raw-exprs)
  (-> (and/c  (*list/c names-def-pair?)
              unencoded?)
      (list/c (*list/c names-def-pair?)
              replacements?))
  (match (foldl mk-output (list '() (hash)) raw-exprs)
    [(list defs reps)
     (list (reverse defs)
           (names-to-reps (hash-values reps)))]))

(module+ test
  (def-test-case "Can find redundancies from definitions list"
    (define defs
      (map split-off-names '((declare-datatypes ()
                                                ((Nat1 (Z1) (S1 (p1 Nat1)))))
                             (declare-datatypes ()
                                                ((Nat2 (Z2) (S2 (p2 Nat2))))))))

    (check-equal? (list (set '(Nat1 Z1 S1 p1)
                             '(Nat2 Z2 S2 p2)))
                  (map list->set (hash-values (second (foldl mk-output
                                                             (list '() (hash))
                                                             defs)))))

    (check-equal? (finalise-replacements
                   (extend-replacements
                    (replacement 'Nat2 'Nat1)
                    (replacement 'Z2   'Z1)
                    (replacement 'S2   'S1)
                    (replacement 'p2   'p1)))
                  (finalise-replacements (second (find-redundancies defs))))

    (check-equal? (set constructorZ constructorS)
                  (list->set (remove-duplicates
                              (replace
                               (finalise-replacements
                                (second
                                 (find-redundancies
                                  (map (compose split-off-names prefix-locals)
                                       redundancies))))
                               redundancies))))))

;; Remove redundancies from EXPRS, leaving only alpha-distinct definitions. When
;; a redundancy is found, we keep the names which appear first lexicographically
;; and update all references to the removed names. Updating these references may
;; cause new redundancies to be exposed: for example APPEND1 and APPEND2 might
;; appear alpha-distinct, since one references LIST1 and the other LIST2; if we
;; find that LIST1 and LIST2 are equivalent, we'll remove LIST2 and replace any
;; references to it with LIST1; this will make APPEND2 equivalent to APPEND1, so
;; it should be removed.
;; For this reason, we keep looping until no more equivalences can be found.
;; Note that this has O(n^3) complexity in the worst case: when all definitions
;; are equivalent, but reference each other such that only two are
;; alpha-equivalent on each pass.
(define/test-contract (norm-defs exprs)
  (-> (*list/c definition?)
      (*list/c definition?))
  (first (normed-and-replacements exprs)))

(module+ test
  (def-test-case "Normalise redundant definitions"
    (define given
      (map prefix-locals
           '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
             (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b)))))

    (define defs  (norm-defs given))
    (define syms  (names-in defs))

    (with-check-info
      (('defs    defs)
       ('syms    syms)
       ('message "Simple redundant functions deduped"))
      (check-true (and      (member 'min1 syms)
                       (not (member 'min2 syms)))))

    (let* ([given (map prefix-locals
                       '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                         (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
                         (define-fun fun3 ((x Int)) Int (min2 x x))))]
           [defs  (norm-defs given)]
           [syms  (symbols-in
                   (filter (lambda (expr)
                             (member 'fun3 expr))
                           defs))])
      (with-check-info
        (('defs    defs)
         ('syms    syms)
         ('message "References to discarded duplicates are replaced"))
        (check-not-equal? (member 'min1 syms) #f)))))

;; Given a list of definitions, returns a set of replacements containing which
;; names are alpha-equivalent to which other names.
;;
;; Note that we take the *transitive closure* when equivalence-checking: for
;; each equivalent pair A and B that we find, we check for equivalence *given
;; that A and B are equivalent*, and so on recursively, until we can't find any
;; more.
(define/test-contract (replacements-closure exprs)
  (-> (*list/c definition?)
      replacements?)
  (second (normed-and-replacements exprs)))

(module+ test
  (define test-benchmark-defs
    (first (normed-and-replacements-cached)))

  (def-test-case "Have replacements"
    (define defs (map prefix-locals
                      '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                        (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b)))))
    (check-equal? (finalise-replacements (replacements-closure defs))
                  (finalise-replacements (replacement 'min2 'min1)))

    (define test-replacements
      (all-replacements-closure))

    (define all-names
      (names-in test-benchmark-defs))

    (for-each (lambda (rep)
                (for-each (lambda (old)
                            (with-check-info
                              (('old   old)
                               ('rep   rep)
                               ('names all-names))
                              (check-false (set-member? all-names old))))
                          (old-of rep))
                (check-not-equal? #f (set-member?
                                      (names-in test-benchmark-defs)
                                      (new-of rep)))

                (define new      (new-of rep))
                (define new-def  (first (get-def-s new test-benchmark-defs)))
                (define norm-new (norm new-def))

                (for-each (lambda (old)
                            (with-check-info
                              (('new new)
                               ('old old)
                               ('rep rep))
                              (check-true (symbol<=? new old)
                                          "New is smaller than old")

                              (check-false (equal? new old)
                                           "New and old are different"))

                            (define old-def
                              (first
                               (get-def-s old
                                          (first (qual-hashes-theorem-files)))))

                            (define norm-old (norm old-def))

                            (define replaced
                              (replace-all (zip (flatten norm-old)
                                                (flatten norm-new))
                                           norm-old))

                            (with-check-info
                              (('rep      rep)
                               ('old      old)
                               ('new      new)
                               ('old-def  old-def)
                               ('new-def  new-def)
                               ('norm-old norm-old)
                               ('norm-new norm-new)
                               ('replaced replaced))
                              (check-equal? norm-new replaced)))
                          (old-of rep)))
              test-replacements)))

;; Do the actual normalisation, to populate the cache
(define (gen-normed-and-replacements)
  (when (getenv "BENCHMARKS_NORMALISED_DEFINITIONS")
    (error (string-append "Shouldn't call gen-normed-and-replacements "
                          "when BENCHMARK_NORMALISED_DEFINITIONS is set")))
  (normed-and-replacements
   (add-custom-defs (first (qual-hashes-theorem-files)))))

;; Removes redundant alpha-equivalent definitions from EXPRS, resulting in a
;; normalised form given as the first element of the result.
;; Also keeps track of the replacements it's made in the process, returning them
;; as the second element of the result.
(define/test-contract (normed-and-replacements exprs)
  (-> (*list/c definition?)
      (list/c (and/c (*list/c definition?)
                     (lambda (defs)
                       (equal? (names-in defs)
                                 (remove-duplicates (names-in defs)))))
              replacements?))

  ;; This does the real work
  (define/test-contract (normed-and-replacements-inner e-len exprs reps)
    (-> integer?
        (*list/c names-def-pair?)
        (*list/c replacements?)
        (list/c (*list/c names-def-pair?) (*list/c replacements?)))

    (msg "Normalising ~a definitions\n" e-len)

    (match (find-redundancies exprs)
      [(list remaining redundancies)
       (msg "Found ~a redundancies\n" (count-replacements redundancies))

       ;; Switch out all of the redundant names (including in definitions)
       (define finalised (finalise-replacements redundancies))

       (define/test-contract stripped
         (*list/c names-def-pair?)
         (replace finalised remaining))

       (define s-len (length stripped))

       (msg "Removed ~a redundant definitions\n" (- e-len s-len))

       (define replacements (cons redundancies reps))

       (if (equal? e-len s-len)
           (list                                stripped replacements)
           (normed-and-replacements-inner s-len stripped replacements))]))

  ;; Remove redundancies from all the given expressions. For efficiency we first
  ;; split all definitions into their normalised form + names.
  (define result
    (normed-and-replacements-inner (length exprs)
                                   (map split-off-names exprs)
                                   '()))

  ;; For each resulting definition, we "plug" the names back into the normalised
  ;; form, and we combine together all of the discovered replacements (this is
  ;; faster than combining them as we go, since it involves sorting).
  (list (map plug-in-names (first result))
        (apply extend-replacements (second result))))

(define (split-off-names expr)
  (define names (toplevel-names-in expr))
  (list names (norm-names-in names expr)))

(define (plug-in-names x)
  (replace-all (zip (toplevel-names-in (second x)) (first x))
               (second x)))

(define (normed-and-replacements-cached)
  (read-from-cache! "BENCHMARKS_NORMALISED_DEFINITIONS"
                    (lambda ()
                      (error "No BENCHMARKS_NORMALISED_DEFINITIONS"))))

(define (mk-final-defs)
  (list->lines (final-benchmark-defs)))

;; Takes a hashmap of filename->content and returns a combined, normalised TIP
;; benchmark
(define (mk-final-defs-hash given-hashes)
  (prepare (norm-defs (first (qual-all-hashes given-hashes)))))

(module+ test
  (def-test-case "mk-final-defs-hash works"
    (check-equal? (mk-final-defs-hash
                   (hash "foo/bar.smt2" `((,nat-def
                                           (define-fun redundantZ1 () Nat
                                             (as Z Nat))
                                           (define-fun redundantZ2 () Nat
                                             (as Z Nat))
                                           (define-fun redundantZ3 () Nat
                                             (as Z Nat)))
                                          (assert-not (= 1 1)))))
                  `((declare-datatypes ()
                      ((Global666f6f2f6261722e736d74324e6174
                        (Global666f6f2f6261722e736d74325a)
                        (Global666f6f2f6261722e736d743253
                         (global666f6f2f6261722e736d743270
                          Global666f6f2f6261722e736d74324e6174)))))
                    (define-fun global666f6f2f6261722e736d7432726564756e64616e745a31 ()
                      Global666f6f2f6261722e736d74324e6174
                      (as Global666f6f2f6261722e736d74325a
                          Global666f6f2f6261722e736d74324e6174))
                    (define-fun global64657374727563746f722d666f6f2f6261722e736d743270
                      ((local-1 Global666f6f2f6261722e736d74324e6174))
                      Global666f6f2f6261722e736d74324e6174
                      (match local-1
                        (case (Global666f6f2f6261722e736d743253 local-2)
                          local-2)))
                    (define-fun global636f6e7374727563746f722d666f6f2f6261722e736d74325a ()
                      Global666f6f2f6261722e736d74324e6174
                      (as Global666f6f2f6261722e736d74325a
                          Global666f6f2f6261722e736d74324e6174))
                    (define-fun global636f6e7374727563746f722d666f6f2f6261722e736d743253
                      ((local-1 Global666f6f2f6261722e736d74324e6174))
                      Global666f6f2f6261722e736d74324e6174
                      (as (Global666f6f2f6261722e736d743253 local-1)
                          Global666f6f2f6261722e736d74324e6174))
                    (check-sat)))))

;; Normalised benchmark from given BENCHMARKS
(memo0 final-benchmark-defs
       (read-from-cache! "BENCHMARKS_FINAL_BENCHMARK_DEFS"
                         (lambda ()
                           (error "No BENCHMARKS_FINAL_BENCHMARK_DEFS"))))

;; All function names defined in given BENCHMARKS. NOTE: These will be
;; hex-encoded.
(memo0 lowercase-benchmark-names
       (lowercase-names (final-benchmark-defs)))

(module+ test
  (define subset
    (replace-names '(grammars/simp_expr_unambig4.smt2nil
                     grammars/simp_expr_unambig4.smt2cons
                     grammars/simp_expr_unambig4.smt2C
                     grammars/simp_expr_unambig4.smt2D
                     grammars/simp_expr_unambig4.smt2X
                     grammars/simp_expr_unambig4.smt2Y
                     grammars/simp_expr_unambig4.smt2Pl
                     grammars/simp_expr_unambig4.smt2Plus
                     grammars/simp_expr_unambig4.smt2EX
                     grammars/simp_expr_unambig4.smt2EY
                     grammars/simp_expr_unambig4.smt2head
                     grammars/simp_expr_unambig4.smt2tail
                     grammars/simp_expr_unambig4.smt2Plus_0
                     grammars/simp_expr_unambig4.smt2Plus_1
                     grammars/simp_expr_unambig4.smt2append
                     grammars/simp_expr_unambig4.smt2linTerm
                     grammars/simp_expr_unambig4.smt2lin

                     isaplanner/prop_84.smt2CustomTrue
                     isaplanner/prop_84.smt2CustomFalse
                     isaplanner/prop_84.smt2custom-bool-converter
                     isaplanner/prop_84.smt2nil
                     isaplanner/prop_84.smt2cons
                     isaplanner/prop_84.smt2Pair2
                     isaplanner/prop_84.smt2Z
                     isaplanner/prop_84.smt2S
                     isaplanner/prop_84.smt2zip
                     isaplanner/prop_84.smt2take
                     isaplanner/prop_84.smt2len
                     isaplanner/prop_84.smt2drop
                     isaplanner/prop_84.smt2append)))

  (def-test-case "No alpha-equivalent duplicates in result"
    (let ([normalised (map norm test-benchmark-defs)])
      (for-each (lambda (norm)
                  (define norms
                    (filter (curry equal? norm) normalised))
                  (unless (equal? norm '(check-sat))
                    (with-check-info
                     (('norms      norms)
                      ('norm       norm)
                      ('normalised normalised)
                      ('message    "Duplicate normalised forms!"))
                     (check-equal? norms (list norm)))))
                normalised)))

  (define qual (first (qual-hashes-theorem-files)))

  (let ([syms (map norm-name (names-in qual))])

    (for-each (lambda (sym)
                (with-check-info
                  (('sym     sym)
                   ('message "Native symbol was stripped"))
                  (check-false (member sym syms))))
              '(true false ite or))

    (for-each (lambda (sym)
                (with-check-info
                  (('sym     sym)
                   ('syms    syms)
                   ('message "Found symbol"))
                  (check-not-equal? (member sym syms)
                                    #f)))
              subset))

  (define (decode-syms x)
    (read-benchmark (decode-string (format-symbols x))))

  ;; For things like custom...
  (define unenc-final (normed-qualified-theorem-files))

  (for-each (lambda (sym)
    (define def
      (get-def-s sym
                 (if (is-custom? sym)
                     unenc-final
                     qual)))

    (let ([count (length def)])
      (with-check-info
       (('sym     sym)
        ('def     def)
        ('count   count)
        ('message "Symbol got qualified"))
       (check-equal? count 1)))

    (define norm-def
      (get-def-s sym test-benchmark-defs))

    (define norm-count
      (length norm-def))

    (with-check-info
     (('sym      sym)
      ('norm-def norm-def)
      ('message  "Got single definition"))
     (check-true (< norm-count 2)))

    (when (equal? norm-count 1)
      ;; The symbols in norm-def may be replacements, so we can't compare
      ;; directly. Instead, we just infer the structure:
      (define (strip-non-paren s)
        (list->string (filter (lambda (c)
                                (or (equal? c #\()
                                    (equal? c #\))))
                              (string->list (format-symbols s)))))

      (define def-shape  (strip-non-paren      def))
      (define norm-shape (strip-non-paren norm-def))

      (with-check-info
       (('def        def)
        ('norm-def   norm-def)
        ('def-shape  def-shape)
        ('norm-shape norm-shape)
        ('message    "Duplicate removal kept definition intact"))
       (check-equal? def-shape norm-shape))))
    (take (shuffle subset) 5))

  (def-test-case "Smallest name chosen"
    ;; The names which appear after normalising should be the first,
    ;; lexicographically, from each alpha-equivalent group

    (define raw       (first (qual-hashes-theorem-files)))
    (define raw-names (names-in raw))

    ;; Remove redundancies
    (define normal       (final-benchmark-defs))
    (define normal-names (names-in normal))

    ;; If some raw name is smaller than some normalised name, the two names must
    ;; not refer to alpha-equivalent definitions (since that would mean the
    ;; normalised name isn't the smallest)
    (for-each (lambda (raw-name)
                ;; Find the definition of this raw name, and all the other names
                ;; which accompany it (e.g. types with constructors and
                ;; destructors, or mutually-recursive functions).

                ;; We do this outside the inner loop for efficiency
                (define raw-def
                  (first (get-def-s raw-name raw)))

                (define norm-raw-def
                  (norm raw-def))

                (define raw-def-names
                  (set->list (names-in raw-def)))

                ;; Compare to all normalised names
                (for-each (lambda (normal-name-enc)
                            ;; Find the definition of this normalised name
                            (define normal-def
                              (first (get-def-s normal-name-enc
                                                normal)))

                            (define norm-normal-def
                              (norm normal-def))

                            ;; We only care when these definitions are
                            ;; alpha-equivalent
                            (when (equal? norm-raw-def norm-normal-def)

                              ;; All of the names defined in the normal version
                              ;; should be <= their equivalents in the raw
                              ;; version

                              (define normal-def-names
                                (map decode-name (names-in normal-def)))

                              (check-equal? (length raw-def-names)
                                            (length normal-def-names))

                              (for-each (lambda (raw-normal-pair)
                                          (define raw
                                            (first raw-normal-pair))

                                          (define normal
                                            (second raw-normal-pair))

                                          (with-check-info
                                            (('raw             raw)
                                             ('normal          normal)
                                             ('raw-name        raw-name)
                                             ('normal-name-enc normal-name-enc)
                                             ('normal-name    (decode-name
                                                               normal-name-enc))
                                             ('raw-def         raw-def)
                                             ('normal-def      normal-def)
                                             ('norm-raw-def    norm-raw-def)
                                             ('norm-normal-def norm-normal-def))
                                            (check-true
                                             (string<=? (symbol->string normal)
                                                        (symbol->string raw)))))
                                        (zip raw-def-names normal-def-names))))
                          normal-names))
              raw-names)

    (check-equal? (finalise-replacements
                   (extend-replacements
                    (replacement 'tip2015/sort_StoogeSort2IsSort.smt2Pair
                                 'isaplanner/prop_58.smt2Pair)
                    (replacement 'tip2015/sort_StoogeSort2IsSort.smt2Pair2
                                 'isaplanner/prop_58.smt2Pair2)
                    (replacement 'tip2015/sort_StoogeSort2IsSort.smt2first
                                 'isaplanner/prop_58.smt2first)
                    (replacement 'tip2015/sort_StoogeSort2IsSort.smt2second
                                 'isaplanner/prop_58.smt2second)))
                  (finalise-replacements
                   (second
                    (find-redundancies
                     (map (compose split-off-names prefix-locals)
                          '((declare-datatypes
                             (local-a local-b)
                             ((isaplanner/prop_58.smt2Pair
                               (isaplanner/prop_58.smt2Pair2
                                (isaplanner/prop_58.smt2first local-a)
                                (isaplanner/prop_58.smt2second local-b)))))
                            (declare-datatypes
                             (local-a local-b)
                             ((tip2015/sort_StoogeSort2IsSort.smt2Pair
                               (tip2015/sort_StoogeSort2IsSort.smt2Pair2
                                (tip2015/sort_StoogeSort2IsSort.smt2first local-a)
                                (tip2015/sort_StoogeSort2IsSort.smt2second local-b))))))))))))

    (define qualified-example
    '((define-fun (par (a b)
                       (foo.smt2baz ((x Nat)) Nat
                                             X)))

      (define-fun
        foo.smt2quux () Bool (hello world))

      (define-fun-rec (par (a)
                           (bar.smt2quux () Foo
                                                  (foo bar))))))

  (def-test-case "Check unqualify"
    (check-equal? (unqualify qualified-example)
                  '((define-fun (par (a b)
                                     (foo.smt2baz  ((x Nat)) Nat
                                                   X)))
                    (define-fun
                      foo.smt2quux ()        Bool
                      (hello world))
                    (define-fun-rec (par (a)
                                         (bar.smt2quux ()        Foo
                                                       (foo bar)))))))

  (def-test-case "Check prepare"
    (check-equal? (prepare qualified-example)
                  `((define-fun (par (a b)
                                     (,(encode-lower-name 'foo.smt2baz)
                                      ((x Nat)) Nat
                                      X)))
                    (define-fun
                      ,(encode-lower-name 'foo.smt2quux)
                      () Bool
                      (hello world))
                    (define-fun-rec (par (a)
                                         (,(encode-lower-name 'bar.smt2quux)
                                          () Foo
                                          (foo bar))))
                    (check-sat))))

  (def-test-case "Find redundancies"
    (check-equal? (finalise-replacements
                   (second (find-redundancies
                            (map (compose split-off-names
                                          prefix-locals)
                                 redundancies))))
                  (finalise-replacements
                   (extend-replacements
                    (replacement 'constructor-S)
                    (replacement 'redundantZ1 'constructor-Z)
                    (replacement 'redundantZ2 'constructor-Z)
                    (replacement 'redundantZ3 'constructor-Z)))
                  "Check known expression")

    (check-equal? (finalise-replacements
                   (second
                    (find-redundancies
                     (map split-off-names
                          '((declare-datatypes () ((TB (C1A) (C2C (D1B TB)))))
                            (declare-datatypes () ((TC (C1C) (C2B (D1A TC)))))
                            (declare-datatypes () ((TA (C1B) (C2A (D1C TA))))))))))
                  (finalise-replacements
                   (extend-replacements
                    (replacement 'TA  'TB  'TC)
                    (replacement 'C1A 'C1B 'C1C)
                    (replacement 'C2A 'C2B 'C2C)
                    (replacement 'D1A 'D1B 'D1C)))
                  "Ensure we keep the lexicographically-smallest name"))

  (def-test-case "Normalise"
    (define (check-normal kind def expected)
      (define canon
        (norm (prefix-locals def)))

      (with-check-info
       (('kind     kind)
        ('def      def)
        ('expect   expected)
        ('canon    canon)
        ('message  "Normalising as expected"))
       (check-equal? canon expected)))

    (check-normal "function"
                  '(define-fun sort2
                     ((x Int) (y Int))
                     (list Int)
                     (ite (<= x y)
                          (cons x (cons y
                                        (as nil (list Int))))
                          (cons y (cons x
                                        (as nil (list Int))))))
                  '(define-fun defining-name-1
                     ((local-1 Int) (local-2 Int))
                     (list Int)
                     (ite (<= local-1 local-2)
                          (cons local-1 (cons local-2
                                                      (as nil (list Int))))
                          (cons local-2 (cons local-1
                                                      (as nil (list Int)))))))

    (check-normal "parameterised function"
                  '(define-fun
                     (par (a)
                          (zsplitAt
                           ((x Int)
                            (y (list a)))
                           (Pair (list a) (list a))
                           (Pair2 (ztake x y)
                                  (zdrop x y)))))
                  '(define-fun
                     (par (local-1)
                          (defining-name-1
                            ((local-2 Int)
                             (local-3 (list local-1)))
                            (Pair (list local-1) (list local-1))
                            (Pair2 (ztake local-2 local-3)
                                   (zdrop local-2 local-3))))))

    (check-normal "datatype"
                  '(declare-datatypes
                    (a)
                    ((list
                      (nil)
                      (cons (head a)
                            (tail (list a))))))
                  '(declare-datatypes
                    (local-1)
                    ((defining-name-1
                       (defining-name-2)
                       (defining-name-3
                         (defining-name-4 local-1)
                         (defining-name-5 (defining-name-1 local-1)))))))

    (check-normal "let binding"
                  '(define-fun-rec msorttd
                     ((x (list Int))) (list Int)
                     (let ((k (div (zlength x) 2)))
                       (lmerge (msorttd (ztake k
                                               x))
                               (msorttd (zdrop k
                                               x)))))
                  '(define-fun-rec defining-name-1
                     ((local-1 (list Int))) (list Int)
                     (let ((local-2 (div (zlength local-1) 2)))
                       (lmerge (defining-name-1 (ztake local-2 local-1))
                               (defining-name-1 (zdrop local-2 local-1))))))

    (check-normal "pattern match"
                  '(define-fun-rec s
                     ((x Bin)) Bin
                     (match x
                       (case One (ZeroAnd One))
                       (case (ZeroAnd xs)
                         (OneAnd xs))
                       (case (OneAnd ys)
                         (ZeroAnd (s ys)))))
                  '(define-fun-rec defining-name-1
                     ((local-1 Bin)) Bin
                     (match local-1
                       (case One (ZeroAnd One))
                       (case (ZeroAnd local-2)
                         (OneAnd local-2))
                       (case (OneAnd local-3)
                         (ZeroAnd (defining-name-1 local-3))))))

    (check-normal "anonymous function"
                  '(define-fun-rec qsort
                     ((y Int) (xs (list Int)))
                     (list Int)
                     (append (append (qsort
                                      (filter (lambda ((z Int))
                                                (<= z
                                                    y))
                                              xs))
                                     (cons y (as nil (list Int))))
                             (qsort
                              (filter (lambda ((x2 Int))
                                        (> x2 y))
                                      xs))))
                  '(define-fun-rec defining-name-1
                     ((local-1 Int) (local-2 (list Int)))
                     (list Int)
                     (append (append (defining-name-1
                                       (filter (lambda ((local-3 Int))
                                                 (<= local-3
                                                     local-1))
                                               local-2))
                                     (cons local-1 (as nil (list Int))))
                             (defining-name-1
                               (filter (lambda ((local-4 Int))
                                         (> local-4 local-1))
                                       local-2))))))

    (def-test-case "Can prepare form"
      (check-true (begin
                    (prepare form-with-deps)
                    #t)))

  (def-test-case "Form defines datatype"
    (define prepared
      (prepare form-with-deps))

    (with-check-info
      (('prepared prepared)
       ('message  "Prepared 'Form' input defines a datatype"))
      (check-not-equal? #f
                        (member 'declare-datatypes (flatten prepared)))))

  (def-test-case "Mutual recursion"
    (define prepared (prepare mut))

    (with-check-info
      (('prepared prepared)
       ('message "Mutually-recursive function definitions survive preparation"))
      (check-not-equal? #f
                        (member 'define-funs-rec (flatten prepared)))))

  (def-test-case "Can find constructors"
    (for-each (lambda (c)
                (define found
                  (get-def-s c test-benchmark-defs))
                (with-check-info
                 (('found found))
                 (check-equal? (length found) 1)))
              (expression-constructors test-benchmark-defs)))

  (define one-liners
    (first (qual-all-hashes (files-to-hashes (list (benchmark-file "isaplanner/prop_84.smt2"))))))

  (define result
    (filter non-empty?
            (map (lambda (expr)
                   (filter non-empty? (names-in (list expr))))
                 one-liners)))

  (let ([all-result (filter non-empty? (map (lambda (x)
                                              (names-in x))
                                            one-liners))])
    (with-check-info
     (('one-liners one-liners)
      ('result     result)
      ('all-result all-result))
     (check-equal? all-result result)))

  (let* ([file "tip2015/sort_StoogeSort2IsSort.smt2"]
         [defs (norm-defs (first (qual-all-hashes (files-to-hashes (list (benchmark-file file))))))])
    (for-each (lambda (data)
                (define name      (first data))
                (define qualified (string-append file name))
                (define def       (toplevel-function-defs-of qualified defs))

                (with-check-info
                 (('def       def )
                  ('name      name)
                  ('kind      (second data))
                  ('qualified qualified)
                  ('message   "Can get function definition")
                  ('defs      defs))
                 (check-equal? (length def) 1)))
              '(("sort2"        "plain")
                ("insert2"      "recursive")
                ("zsplitAt"     "parameterised")
                ("ztake"        "parameterised recursive")
                ("stooge2sort2" "mutually recursive"))))

  (def-test-case "Real symbols qualified"
    (define fs (benchmark-files '("tip2015/propositional_AndCommutative.smt2")))
    (define hashes (files-to-hashes fs))
    (define q (first (qual-all-hashes hashes)))
    (define s (names-in q))

    (check-true (string-contains? (~a s) "or2")
                "Found an or2 symbol")

    (check-false (member 'or2 s)
                 "or2 symbol is qualified")

    (check-true (string-contains? (format-symbols
                                   (names-in
                                    (norm-defs (first (qual-all-hashes hashes)))))
                                  "or2")
                "Found 'or2' symbol"))

  (let ([syms (names-in test-benchmark-defs)])
    (for-each (lambda (sym)
                (define str (string-downcase (symbol->string sym)))

                (with-check-info
                  (('sym     sym)
                   ('str     str)
                   ('message "Symbol is qualified"))
                 (check-true (or (string-contains? str ".smt2")
                                 (is-custom? sym)))))
              syms)))

(memo0 normed-qualified-theorem-files
       (preprepare (first (normed-and-replacements-cached))))

(define/test-contract (constructor-function-replacements qualified replacements)
  (-> (*list/c definition?) replacements? replacements?)

  (define all-constructors
    (expression-constructors qualified))

  (apply extend-replacements
         (map (lambda (constructor)
                (replacement constructor
                             (prefix-name (unqualify (norm-name constructor))
                                          "constructor-")))
              all-constructors)))

(define (all-constructor-function-replacements)
  (constructor-function-replacements (first (qual-hashes-theorem-files))
                                     (all-replacements-closure)))

(module+ test
  (define/test-contract (constructor-function-replacements-from-hashes hashes)
    (-> tip-benchmarks?
        replacements?)

    (define qualified
      (first (qual-all-hashes hashes)))

    (define replacements
      (replacements-closure qualified))

    (constructor-function-replacements qualified replacements))

  (def-test-case "Constructor function name replacements"
    (check-equal? (finalise-replacements
                   (extend-replacements
                    (replacement 'prod/prop_35.smt2Z
                                 (prefix-name (nn 'prod/prop_35.smt2Z)
                                              "constructor-"))
                    (replacement 'prod/prop_35.smt2S
                                 (prefix-name (nn 'prod/prop_35.smt2S)
                                              "constructor-"))))
                  (finalise-replacements
                   (constructor-function-replacements-from-hashes
                    (hash "prod/prop_35.smt2"
                          `((,nat-def)
                            (assert-not (= 1 1)))))))))

(define (all-replacements-closure)
  (second (normed-and-replacements-cached)))

(memo0 name-replacements
       (foldl (lambda (x h)
                (hash-set h x x))
              (finalise-replacements (all-replacements-closure))
              '(CustomInt CustomNeg CustomZero CustomPos
                CustomNat CustomZ CustomS
                CustomBool CustomTrue CustomFalse
                custom-bool-converter)))

(memo0 all-names
  (append-map toplevel-names-in (first (qual-hashes-theorem-files))))

(memo0 final-constructor-names
       (filter (lambda (name) (string-prefix? (symbol->string name)
                                              "constructor-"))
               (names-in (normed-qualified-theorem-files))))

(define/test-contract (norm-name x)
  (-> (and/c symbol?
             (lambda (x)
               (or (member x (all-names))
                   (is-custom? x)
                   (member x (final-constructor-names))
                   (raise-arguments-error
                    'norm-name
                    "Name not found"
                    "given" x
                    "available" (all-names)))))
      (and/c symbol?
             (lambda (x)
               (define final-names
                 (map decode-name (names-in (final-benchmark-defs))))

               (or (member x final-names)
                   (raise-user-error
                    'norm-name
                    "Result '~a' isn't in names:\n~a\n"
                    x
                    final-names)))))
  (cond
    ;; custom* and Custom* can be left unchanged
    [(string-prefix? (string-downcase (symbol->string x)) "custom") x]

    ;; Leave constructor functions as-is
    #;[(string-prefix? (symbol->string x) "constructor-") x]

    ;; Names appearing in name-replacements are looked up (which may yield
    ;; themselves)
    [(hash-has-key? (name-replacements) x)
     (hash-ref (name-replacements) x)]

    ;; Other names refer to themselves
    [else x]))

;; Shorthand, since it's used to often
(define nn norm-name)

(define (replace-names x)
  (replace (name-replacements) x))

(memo0 qualified-theorems
       (second (qual-hashes-theorem-files)))

(module+ test
  (def-test-case "Theorems are qualified"
    (hash-for-each (qualified-theorems)
                   (lambda (pth thm)
                     (for-each (lambda (sym)
                                 (define str (symbol->string sym))

                                 (unless (string-contains? str "ustom")
                                   (with-check-info
                                     (('sym sym)
                                      ('thm thm))
                                     (check-true (string-contains? str ".smt2")
                                                 "Symbol is qualified"))))
                               (symbols-in thm))))))
