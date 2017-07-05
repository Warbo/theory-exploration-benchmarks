#lang racket

;; Normalise TIP definitions

(require racket/trace)
(require "compare.rkt")
(require "impure.rkt")
(require "lists.rkt")
(require "memo.rkt")
(require "replacements.rkt")
(require "tip.rkt")
(require "util.rkt")

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
  (require "testing.rkt"))

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

;; Normalise an expression: all type parameters, local variables, global
;; definitions, etc. are replaced with sequential names. References to global
;; names are left intact. This allows easy alpha-equivalence checking: A and B
;; are alpha-equivalent iff (equal? (norm A) (norm B))
(define/test-contract norm
  (-> any/c
      ;; Our result should be normalised
      (lambda (result)
        (all-of (lambda (name)
                  (or (regexp-match? "^defining-"  (symbol->string name))
                      (regexp-match? "^normalise-" (symbol->string name))
                      (raise-user-error
                       'norm
                       "Name '~a' wasn't normalised away in:\n~a\n"
                       name
                       result)))
                (names-in result))))
  (memo1 (lambda (expr)
  (define norm-func-1 'defining-function-1)

  (define (norm-func args body)
    (if (empty? args)
        (list args (norm body))
        (let* ([arg (car args)]
               [rec (norm-func (cdr args) body)]
               [v   (next-var rec)])
          (list (cons (cons v (cdr arg)) (first rec))
                (replace-in (car arg) v (second rec))))))

  (define (norm-params ps def)
    (if (empty? ps)
        (match def
          [(list name        args        return body)
           (let ([fun (norm-func args body)])
             (list ps
                   (list norm-func-1 (first fun) return
                         (replace-in name norm-func-1  (second fun)))))]
          [_ (error "Unexpected parameterised function definition" def)])
        (let* ([p   (car ps)]
               [rec (norm-params (cdr ps) def)]
               [v   (next-var rec)])
          (list (cons v (first rec))
                (replace-in p v (second rec))))))

  (match expr
    [(list 'define-funs-rec decs defs)
     (begin
       (define (norm-mutual dec def)
         ;; Declarations may or may not be parameterised
         (define par  (equal? 'par (first dec)))
         (define pars (when par
                        (second dec)))

         ;; Pair up each declaration with its definition and treat like a
         ;; regular function
         (define new (if par
                         (norm-func (second (third dec))
                                    def)
                         (norm-func (second dec)
                                    def)))

         (define new-pars
           (when par
             (second
              (foldl (lambda (par name-result)
                       (define name   (first  name-result))
                       (define result (second name-result))
                       (list (next-var name)
                             (cons (list par name) result)))
                     (list (next-var new)
                           '())
                     pars))))

         (when par
           (unless (equal? (length pars) (length new-pars))
             (raise-user-error
              'norm-mutual
              "Normalising parameters shouldn't change how many there are. Given:\n~a\nProduced:\n~a\n"
              pars
              new-pars)))

         (define final
           (if par
               (replace-all new-pars new)
               new))

         (define final-args
           (first final))

         (define name
           (if par
               (first (third dec))
               (first dec)))

         (define type
           (if par
               (replace-all new-pars (third (third dec)))
               (third dec)))

         (list
          (if par
              ;;         parameters
              (list 'par (map second new-pars)
                    (list name final-args type))

              (list name final-args type))

          ;; Body
          (second final)))

       (define new-decs-defs
         (map (lambda (dec-def)
                (norm-mutual (first dec-def) (second dec-def)))
              (zip decs defs)))

       ;; Split the results back up into declarations and definitions
       (define new-decs
         (map first new-decs-defs))

       (unless (equal? (length decs) (length new-decs))
         (raise-user-error
          'norm
          "Normalising recursive functions should keep the same number of declarations, got:\n~a\nproduced:\n~a\n"
          decs
          new-decs))

       (define new-defs
         (map second new-decs-defs))

       (unless (equal? (length defs) (length new-defs))
         (raise-user-error
          'norm
          "Normalising recursive functions should keep the same number of definitions, got:\n~a\nproduced:\n~a\n"
          defs
          new-defs))

       ;; Invent a "defining-function-X" name for each name being declared
       (define names
         (names-in expr))

       (define new-names
         (foldl
          (lambda (name result)
            (cons (list name
                        (string->symbol
                         (string-append "defining-function-"
                                        (~a (+ 1 (length result))))))
                  result))
          '()
          names))

       ;; Replace all of the declared names in the normalised result
       (define result
         (replace-all new-names
                      (list 'define-funs-rec new-decs new-defs)))

       result)]

    [(  list 'define-fun-rec (list 'par p         def))
     (let ([rec (norm-params p def)])
       (list 'define-fun-rec (list 'par (first rec) (second rec))))]

    [(  list 'define-fun-rec name        args      return body)
     (let ([rec       (norm-func args body)])
       (list 'define-fun-rec norm-func-1 (first rec) return (replace-in name
                                                                      norm-func-1
                                                                      (second rec))))]

    [(  list 'define-fun (list 'par p         def))
     (let ([rec (norm-params p def)])
       (list 'define-fun (list 'par (first rec) (second rec))))]

    [(  list 'define-fun name        args      return body)
     (let ([rec (norm-func args body)])
       (list 'define-fun norm-func-1 (first rec) return (replace-in name
                                                                  norm-func-1
                                                                  (second rec))))]

    [  (list 'declare-datatypes given     decs)
     (define (replace-param p expr)
       (define name
         (inc-name var-prefix (max-name var-prefix expr)))

       (list (cons name (first expr))
             (replace-in p name (second expr))))

     (define (norm-type dec expr)
       (define type-prefix "defining-type-")

       (define name
         (inc-name type-prefix (max-name type-prefix expr)))

       (define (norm-constructor c rec3)
         (define (norm-destructor d rec2)
           (define destructor-prefix "normalise-destructor-")

           (cons (cons (inc-name destructor-prefix
                                 (max-name destructor-prefix
                                           (list expr rec3 rec2)))
                       (cdr d))
                 rec2))

         (define constructor-prefix "normalise-constructor-")

         (cons (cons (inc-name constructor-prefix
                               (max-name constructor-prefix (list expr rec3)))
                     (foldr norm-destructor '() (cdr c)))
               rec3))

       (cons (cons name
                   (replace-in (car dec) name
                               (foldr norm-constructor '() (cdr dec))))
             expr))

     (cons 'declare-datatypes
           (foldr replace-param
                  (list '() (foldr norm-type '() decs))
                  given))]

    [  (list 'case pat body)
     (let ([norm-body (norm body)])
       (cons 'case (match pat
                     [(list con)    (list pat norm-body)]
                     [(cons con ps) (match (foldl (lambda (x y)
                                                    (let ([name (next-var y)])
                                                      (list (cons name (first y))
                                                            (replace-in x name (second y)))))
                                                  (list '() norm-body)
                                                  ps)
                                      [(list norm-ps norm-body2)
                                       (list (cons con norm-ps) norm-body2)])]
                     [_             (list pat norm-body)])))]

    [(list 'lambda args body)
       (cons 'lambda (norm-func args body))]

    [(list 'let bindings body)
     (cons 'let (foldr (lambda (binding rec)
                         (let* ([value (norm (second binding))]
                                [name  (next-var (cons value rec))])
                           (list (cons (list name value) (first rec))
                                 (replace-in (first binding) name (second rec)))))
                       (list '() (norm body))
                       bindings))]

    [(cons a b) (cons (norm a) (norm b))]

    [_ expr]))))

(module+ test
  (def-test-case "Norm"
    (check-equal? '(declare-datatypes
                    ()
                    ((defining-type-1
                       (normalise-constructor-2)
                       (normalise-constructor-1
                        (normalise-destructor-1 defining-type-1)))))
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

;; Prefix each name with the path of the file it came from, and combine all
;; definitions together into one long list (ordered lexicographically by path).
;; Theorems are also qualified, but remain independent and keyed by their path.
(define/test-contract (qual-all-hashes given-hashes)
  (-> tip-benchmarks? (list/c (*list/c definition?)
                              qualified-theorems?))
  (define entries
    (sort (hash->list given-hashes) string>? #:key car))

  (define result
    (foldl (lambda (elem result)
             (match (list elem result)
               [(list (cons pth (list defs thm))
                      (list all thms))
                (list (cons (qualify pth defs) all)
                      (hash-set thms pth (qualify pth thm)))]))
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
                  "Prefixing is idempotent")))

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

    func)

  (append x (map func-for (expression-constructors x))))

(module+ test
  (def-test-case "Can add constructor functions"
    (check-equal? (add-constructor-funcs (list nat-def))
                  `(,nat-def ,constructorZ ,constructorS)))

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
                      (par (local-a)
                           (constructor-MyCons
                            ((local-myHead local-a)
                             (local-myTail (MyStream local-a)))
                            (MyStream local-a)
                            (as (MyCons local-myHead local-myTail)
                                (MyStream local-a)))))))))

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

    (replace-all (zip unprefixed-parameters
                      (map prefix-local unprefixed-parameters))
                 func))

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
                    (define-fun destructor-p ((destructor-arg Nat)) Nat
                      (match destructor-arg
                        (case (S local-p) local-p))))))

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
                      (par (local-a local-b)
                           (destructor-theOne
                            ((destructor-arg (OneAndMany local-a local-b)))
                            local-a
                            (match destructor-arg
                              (case (One local-theOne) local-theOne)))))
                    (define-fun
                      (par (local-a local-b)
                           (destructor-head
                            ((destructor-arg (OneAndMany local-a local-b)))
                            local-b
                            (match destructor-arg
                              (case (Many local-head local-tail) local-head)))))
                    (define-fun
                      (par (local-a local-b)
                           (destructor-tail
                            ((destructor-arg (OneAndMany local-a local-b)))
                            (OneAndMany local-a local-b)
                            (match destructor-arg
                              (case (Many local-head local-tail)
                                local-tail)))))))))

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
                         (Global53 (global70 Global4e6174)))))))))

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

            ;; Loop once for each element in the lists (e.g. n1, n2, n3, ...).
            ;; 'included' will be updated each iteration, and returned as result
            (for/fold ([included result])
                      ([_ (first equivs)] ;; stop when names run out
                       [n (in-naturals 0)]) ;; names to add this iteration

              ;; Get nth element of each list in equivs
              (let ([nths (map (curry nth n) equivs)])

                ;; Turn into replacement set and accumulate
                (cons (apply replacement nths) included))))
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
  (define ((names-list? len) names)
    (and (list? names)
         (not (empty? names))
         (equal? (length names) len)))

  (define names-expr?
    (match-lambda
      [(list namess expr)
       (and (definition? expr)
            (list? namess)
            (not (empty? namess))
            (all-of (names-list? (length (first namess))) namess))]))

  (define names-expr-pair?
    (lambda (x)
      (and (list? x)
           (equal? 2 (length x))
           (names-expr? x))))

  (and (list? x)
       (all-of names-expr-pair? x)))

(define/test-contract (mk-output expr so-far)
  (-> definition?
      normalised-intermediate?
      normalised-intermediate?)

  (define/test-contract norm-line
    definition?
    (norm expr))

  (define/test-contract names
    (*list/c symbol?)
    (toplevel-names-in expr))

  ;; Look for an existing alpha-equivalent definition
  ;;   '(((name1 name2 ...) expr) ...)
  (define/test-contract existing-pos
    (or/c integer? boolean?)
    (index-where so-far (lambda (x)
                          (equal? norm-line (second x)))))

  (if (equal? #f existing-pos)
      ;; This expr isn't redundant, associate its names with its normal form
      (cons (list (list names) norm-line)
            so-far)

      ;; This expr is redundant, include its names in the replacement list
      (list-update so-far existing-pos (lambda (existing)
                                         (list (cons names (first existing))
                                               norm-line)))))

(module+ test
  (let ([output (mk-output constructorZ
                           `((((existing-Z)) ,(norm constructorZ))))])
    (def-test-case "Redundant output contains normalised defs"
      (check-equal? (map second output)
                    `(,(norm constructorZ))))

    (def-test-case "Redundant output contains classes of equivalent names"
      (check-equal? (map first output)
                    `(((constructor-Z) (existing-Z)))))

    (def-test-case "Redundancy output"
      (check-equal? output
                    `((((constructor-Z) (existing-Z)) ,(norm constructorZ)))))))

;; Looks for alpha-equivalent definitions in RAW-EXPRS, and returns a set of
;; name replacements which can be used to update references and remove
;; redundancies.
(define/test-contract (find-redundancies raw-exprs)
  (-> (and/c (*list/c definition?) unencoded? unnormalised?)
      replacements?)
  (names-to-reps (map first (foldl mk-output null raw-exprs))))

(module+ test
  (def-test-case "Can find redundancies from definitions list"
    (define defs
      '((declare-datatypes ()
                           ((Nat1 (Z1) (S1 (p1 Nat1)))))
        (declare-datatypes ()
                           ((Nat2 (Z2) (S2 (p2 Nat2)))))))

    (check-equal? (list (set '(Nat1 Z1 S1 p1)
                             '(Nat2 Z2 S2 p2)))
                  (map (lambda (x) (list->set (first x)))
                       (foldl mk-output null defs)))

    (check-equal? (finalise-replacements
                   (extend-replacements
                    (replacement 'Nat2 'Nat1)
                    (replacement 'Z2   'Z1)
                    (replacement 'S2   'S1)
                    (replacement 'p2   'p1)))
                  (finalise-replacements (find-redundancies defs)))

    (check-equal? (set constructorZ constructorS)
                  (list->set (remove-duplicates
                              (replace
                               (finalise-replacements
                                (find-redundancies redundancies))
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
      '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
        (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))))

    (define defs  (norm-defs given))
    (define syms  (names-in defs))

    (with-check-info
      (('defs    defs)
       ('syms    syms)
       ('message "Simple redundant functions deduped"))
      (check-true (and      (member 'min1 syms)
                       (not (member 'min2 syms)))))

    (let* ([given '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                    (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
                    (define-fun fun3 ((x Int)) Int (min2 x x)))]
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
    (define defs '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                   (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))))
    (check-equal? (finalise-replacements (replacements-closure defs))
                  (finalise-replacements (replacement 'min2 'min1)))

    (define test-replacements
      (all-replacements-closure))

    (for-each (lambda (rep)
                (for-each (lambda (old)
                            (check-false (set-member?
                                          (names-in test-benchmark-defs)
                                          old)))
                          (old-of rep))
                (check-not-equal? #f (set-member?
                                      (names-in test-benchmark-defs)
                                      (new-of rep))))
              test-replacements)))

;; Do the actual normalisation, to populate the cache
(define (gen-normed-and-replacements)
  (when (getenv "BENCHMARKS_NORMALISED_DEFINITIONS")
    (error (string-append "Shouldn't call gen-normed-and-replacements "
                          "when BENCHMARK_NORMALISED_DEFINITIONS is set")))
  (normed-and-replacements (first (qual-hashes-theorem-files))))

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
  (define/test-contract (normed-and-replacements-inner exprs reps)
    (-> (*list/c definition?)
        (*list/c replacements?)
        (list/c (*list/c definition?) (*list/c replacements?)))

    (msg "Normalising ~a definitions\n" (length exprs))

    ;; Find the names of redundant definitions, and a canonical replacement
    (define/test-contract redundancies
      replacements?
      (find-redundancies exprs))

    (msg "~a names remain distinct\n" (count-replacements redundancies))

    ;; Switch out all of the redundant names (including in definitions)
    (define/test-contract stripped
      (*list/c definition?)
      (remove-duplicates (replace (finalise-replacements redundancies)
                                  exprs)))

    (define e-len (length exprs))
    (define s-len (length stripped))

    (msg "Removed ~a redundant definitions\n" (- e-len s-len))

    (define replacements (cons redundancies reps))

    (if (equal? e-len s-len)
        (list                          stripped replacements)
        (normed-and-replacements-inner stripped replacements)))

  (define result
    (normed-and-replacements-inner (map prefix-locals exprs)
                                   '()))
  (list (first result)
        (apply extend-replacements (second result))))

(define (normed-and-replacements-cached)
  (read-from-cache! "BENCHMARKS_NORMALISED_DEFINITIONS"
                    (lambda ()
                      (error "No BENCHMARKS_NORMALISED_DEFINITIONS"))))

(define (mk-final-defs)
  (show (final-benchmark-defs)))

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
                      ((destructor-arg Global666f6f2f6261722e736d74324e6174))
                      Global666f6f2f6261722e736d74324e6174
                      (match destructor-arg
                        (case (Global666f6f2f6261722e736d743253 local-p)
                          local-p)))
                    (define-fun global636f6e7374727563746f722d666f6f2f6261722e736d74325a ()
                      Global666f6f2f6261722e736d74324e6174
                      (as Global666f6f2f6261722e736d74325a
                          Global666f6f2f6261722e736d74324e6174))
                    (define-fun global636f6e7374727563746f722d666f6f2f6261722e736d743253
                      ((local-p Global666f6f2f6261722e736d74324e6174))
                      Global666f6f2f6261722e736d74324e6174
                      (as (Global666f6f2f6261722e736d743253 local-p)
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
    (let ([normalised (norm test-benchmark-defs)])
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

  (let ([syms (names-in qual)])

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

  (for-each (lambda (sym)
    (define def (get-def-s sym qual))

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
                   (find-redundancies
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
                          (tip2015/sort_StoogeSort2IsSort.smt2second local-b))))))))))

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
    (check-equal? (finalise-replacements (find-redundancies redundancies))
                  (finalise-replacements
                   (extend-replacements
                    (replacement 'constructor-S)
                    (replacement 'redundantZ1 'constructor-Z)
                    (replacement 'redundantZ2 'constructor-Z)
                    (replacement 'redundantZ3 'constructor-Z)))
                  "Check known expression")

    (check-equal? (finalise-replacements
                   (find-redundancies
                    '((declare-datatypes () ((TB (C1A) (C2C (D1B TB)))))
                      (declare-datatypes () ((TC (C1C) (C2B (D1A TC)))))
                      (declare-datatypes () ((TA (C1B) (C2A (D1C TA))))))))
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
        (norm def))

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
                  '(define-fun defining-function-1
                     ((normalise-var-2 Int) (normalise-var-1 Int))
                     (list Int)
                     (ite (<= normalise-var-2 normalise-var-1)
                          (cons normalise-var-2 (cons normalise-var-1
                                                      (as nil (list Int))))
                          (cons normalise-var-1 (cons normalise-var-2
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
                     (par (normalise-var-3)
                          (defining-function-1
                            ((normalise-var-2 Int)
                             (normalise-var-1 (list normalise-var-3)))
                            (Pair (list normalise-var-3) (list normalise-var-3))
                            (Pair2 (ztake normalise-var-2 normalise-var-1)
                                   (zdrop normalise-var-2 normalise-var-1))))))

    (check-normal "datatype"
                  '(declare-datatypes
                    (a)
                    ((list
                      (nil)
                      (cons (head a)
                            (tail (list a))))))
                  '(declare-datatypes
                    (normalise-var-1)
                    ((defining-type-1
                       (normalise-constructor-2)
                       (normalise-constructor-1 (normalise-destructor-2 normalise-var-1)
                                                (normalise-destructor-1 (defining-type-1 normalise-var-1)))))))

    (check-normal "let binding"
                  '(define-fun-rec msorttd
                     ((x (list Int))) (list Int)
                     (let ((k (div (zlength x) 2)))
                       (lmerge (msorttd (ztake k
                                               x))
                               (msorttd (zdrop k
                                               x)))))
                  '(define-fun-rec defining-function-1
                     ((normalise-var-2 (list Int))) (list Int)
                     (let ((normalise-var-1 (div (zlength normalise-var-2) 2)))
                       (lmerge (defining-function-1 (ztake normalise-var-1
                                                           normalise-var-2))
                               (defining-function-1 (zdrop normalise-var-1
                                                           normalise-var-2))))))

    (check-normal "pattern match"
                  '(define-fun-rec s
                     ((x Bin)) Bin
                     (match x
                       (case One (ZeroAnd One))
                       (case (ZeroAnd xs)
                         (OneAnd xs))
                       (case (OneAnd ys)
                         (ZeroAnd (s ys)))))
                  '(define-fun-rec defining-function-1
                     ((normalise-var-2 Bin)) Bin
                     (match normalise-var-2
                       (case One (ZeroAnd One))
                       (case (ZeroAnd normalise-var-1)
                         (OneAnd normalise-var-1))
                       (case (OneAnd normalise-var-1)
                         (ZeroAnd (defining-function-1 normalise-var-1))))))

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
                  '(define-fun-rec defining-function-1
                     ((normalise-var-3 Int) (normalise-var-2 (list Int)))
                     (list Int)
                     (append (append (defining-function-1
                                       (filter (lambda ((normalise-var-1 Int))
                                                 (<= normalise-var-1
                                                     normalise-var-3))
                                               normalise-var-2))
                                     (cons normalise-var-3 (as nil (list Int))))
                             (defining-function-1
                               (filter (lambda ((normalise-var-1 Int))
                                         (> normalise-var-1 normalise-var-3))
                                       normalise-var-2))))))

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
                (with-check-info
                 (('sym       sym)
                  ('test-benchmark-defs test-benchmark-defs)
                  ('message   "Symbol is qualified"))
                 (check-true (string-contains? (symbol->string sym)
                                               ".smt2"))))
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

(define (norm-name x)
  (cond
    ;; custom* and Custom* can be left unchanged
    [(string-prefix? (string-downcase (symbol->string x)) "custom")
     x]

    ;; Leave constructor functions as-is
    [(string-prefix? (symbol->string x) "constructor-") x]

    ;; Names appearing in name-replacements are looked up (which may yield
    ;; themselves
    [(hash-has-key? (name-replacements) x)
     (hash-ref (name-replacements) x)]

    [else
     (raise-arguments-error 'norm-name
                            "Name not found"
                            "given" x
                            "available" (hash-keys (name-replacements)))]))

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
