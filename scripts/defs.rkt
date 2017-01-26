#lang racket
(require grommet/crypto/hash/sha256)
(require json)
(require racket/contract)
(require racket/contract/combinator)
(require racket/function)
(require racket/match)
(require racket/trace)
(require shell/pipeline)

(provide decode-string)
(provide full-haskell-package)
(provide log)
(provide mk-defs)
(provide mk-final-defs)
(provide mk-signature)
(provide precision-wrapper)
(provide qualify-given)
(provide recall-wrapper)
(provide sample-from-benchmarks)
(provide sample-equational-from-benchmarks)
(provide symbols-of-theorems)
(provide types-from-defs)

(define-values (quiet log)
  (let ([verbose #t])
    (values (lambda ()
              (unless (getenv "DEBUG")
                (set! verbose #f)))

            (lambda args
              (when verbose
                (eprintf (apply format args)))))))

;; Uses 'define/contract' during testing, and 'define' otherwise. Useful since
;; 'define/contract' can be very slow, e.g. checking every recursive call.
(define-syntax (define/test-contract stx)
  (syntax-case stx ()
    [(define/test-contract sig contract body ...)
     (if (and (getenv "PLT_TR_CONTRACTS") #t)
         #'(define/contract sig contract body ...)
         #'(define          sig          body ...))]))

;; Memoise a value, so it's only computed if needed and the result is reused on
;; subsequent calls. Note that the resulting value will be a function, which
;; needs to be called to get the value.
(define-syntax-rule (memo0 name body ...)
  (define name
    (let ([result #f]
          [called #f])
      (lambda ()
        (when (equal? #f called)
          (log "Forced ~a\n" name)
          (set! result (let () body ...))
          (set! called #t)
          (log "Finished ~a\n" name))
        result))))

(define benchmark-dir
  (or (getenv "BENCHMARKS")
      (getenv "BENCHMARKS_FALLBACK")
      (raise-user-error
       'benchmark-dir
       "No BENCHMARKS_FALLBACK env var found; should be set by Nix")))

(define benchmark-file
  (curry string-append benchmark-dir "/"))

(define benchmark-files
  (curry map benchmark-file))

;; Keywords of the TIP format. We specifically *avoid* parts of the TIP
;; specification like Int, Bool and their associated operations, which should be
;; replaced by the strip-native.rkt script. Due to constraints of the TIP format
;; some of these are currently unavoidable, e.g. polymorphic = which produces a
;; Bool; they shouldn't appear outside our wrappers though. Also, => should only
;; be used for function types, use custom-=> for boolean implication.
(define native-symbols
  '(ite Bool = distinct => ;; Should only appear as per strip-native.rkt
    @ as forall assert-not lambda case match let))

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
      [(list 'case pat body) (remove* (symbols-in pat) (symbols-in body))]
      [_                     (error "Unexpected case form")]))

  (remove* native-symbols (match exp
    [(cons 'match (cons arg cases)) (append (symbols-in arg)
                                            (symbols-in (map case-symbols cases)))]
    [(list 'lambda args body)       (remove* (map car args)
                                             (symbols-in body))]
    [(list 'let defs body)          (remove* (map car defs)
                                             (append (symbols-in (map cdr defs))
                                                     (symbols-in body)))]
    [(list 'as val typ)             (append (symbols-in val)
                                            (symbols-in typ))]
    [(cons a b)                     (append (symbols-in a)
                                            (symbols-in b))]
    [_                              (if (symbol? exp) (list exp) null)])))

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

;; Return a list of types defined in a given expression, e.g. '(List) if given a
;; definition of List
(define (expression-types exp)
  (define (constructor-types defs)
    (match defs
      [(cons h t) (append (concat-map (lambda (x) (symbols-in (cdr x)))
                                      (cdr h))
                          (constructor-types t))]
      [_          null]))

  (match exp
         [(list 'define-fun-rec
                (list 'par p
                      (list name args return body)))   (remove* (symbols-in p)
                                                                (symbols-in (cons return (map cadr args))))]
         [(list 'define-fun-rec name args return body) (cons return (map cadr args))]
         [(list 'define-fun
                (list 'par p
                      (list name args return body)))   (remove* (symbols-in p)
                                                                (symbols-in (cons return (map cadr args))))]
         [(list 'define-fun     name args return body) (cons return (map cadr args))]

         [(list 'declare-datatypes given decs)         (append (map car decs)
                                                               (remove* (symbols-in given)
                                                                        (symbols-in (map (lambda (x) (constructor-types (cdr x)))
                                                                                         decs))))]
         [(cons a b)                                   (append (expression-types a)
                                                               (expression-types b))]
         [_                                            null]))

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

;; Returns all global names defined in the given expression, including
;; functions, constructors and destructors, but excluding types
(define (expression-symbols exp)
  (define expression-funs (memo1 (lambda (exp)
    (define (fun-rec-expressions decs defs)
      (match (list decs defs)
        [(list (cons (list 'par ps (list name args return)) more-decs)
               (cons body                                   more-defs))
         (append (cons name (remove* (append ps (map car args))
                                     (symbols-in body)))
                 (fun-rec-expressions more-decs more-defs))]
        [(list (cons (list name args return) more-decs)
               (cons body                    more-defs)) (append (cons name (remove* (map car args)
                                                                                     (symbols-in body)))
                                                                 (fun-rec-expressions more-decs more-defs))]
        [_                                                null]))

    (match exp
      [(list 'define-fun-rec
             (list 'par p
                   (list name args return body)))   (cons name (remove* (append (map car args)
                                                                                (symbols-in p))
                                                                        (symbols-in body)))]
      [(list 'define-fun-rec name args return body) (cons name (remove* (map car args) (symbols-in body)))]
      [(list 'define-funs-rec decs defs)            (fun-rec-expressions decs defs)]
      [(list 'define-fun
             (list 'par p
                   (list name args return body)))   (cons name (remove* (append (map car args)
                                                                                (symbols-in p))
                                                                        (symbols-in body)))]
      [(list 'define-fun     name args return body) (cons name (remove* (map car args) (symbols-in body)))]

      [(cons a b)                                   (append (expression-funs a)
                                                            (expression-funs b))]
      [_                                            null]))))

  (remove* (expression-types exp)
           (append (expression-constructors exp)
                   (expression-destructors  exp)
                   (expression-funs         exp))))

;; Prefix all definitions in EXPR with NAME, and prefix all local variables
(define (qualify name expr)
  (foldl (lambda (sym x)
           (replace-in sym
                       (string->symbol (string-append name
                                                      (symbol->string sym)
                                                      "-sentinel"))
                       x))
         (prefix-locals expr)
         (symbols-in (append (expression-symbols expr)
                             (expression-types   expr)))))

;; Prefixes a local variable name
(define (prefix-local s)
  (string->symbol (string-append "local-" (as-str s))))

;; Prefix all bound local variables appearing in EXPR, including arguments of
;; function definitions and lambda functions, let-bound variables and
;; pattern-match cases
(define (prefix-locals expr)
  ;; Turn bindings like (lambda (x Int) (f x)) into
  ;; (lambda (local-x Int) (f local-x)) to prevent conflicts between local and
  ;; global names (e.g. if there's a destructor called x)
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

      [(cons a b) (append (locals-in a) (locals-in b))]

      [_ null]))

  (define (prefix-all locals expr)
    (if (empty? locals)
        expr
        (prefix-all (cdr locals)
                    (replace-in (car locals)
                                (prefix-local (car locals))
                                expr))))

  (prefix-all (locals-in expr) expr))

;; Replace all occurrences of OLD with REPLACEMENT in EXPR
(define (replace-in old replacement expr)
  (if (equal? old expr)
      replacement
      (match expr
        [(cons a b) (cons (replace-in old replacement a)
                          (replace-in old replacement b))]
        [_          expr])))

;; For each (OLD NEW) in REPS, replace OLD with NEW in EXPR
(define (replace-all reps expr)
  (if (empty? reps)
      expr
      (replace-all (cdr reps)
                   (replace-in (first  (first reps))
                               (second (first reps))
                               expr))))

;; Format a list of expressions to a string, with one expression per line. The
;; list's parens aren't included.
(define (format-symbols syms)
  (if (null? syms)
      ""
      (format "~s\n~a" (car syms) (format-symbols (cdr syms)))))

;; Print a list of expressions to (current-output-port)
(define (show x)
  (displayln (format-symbols x)))

;; Reverse the characters of a string
(define (string-reverse s)
  (list->string (reverse (string->list s))))

;; Memoise a unary function
(define (memo1 init)
  (let ([results (make-hash)])
    (lambda (arg)
      (hash-ref! results
                 arg
                 (lambda () (init arg))))))

;; Memoise a binary function
(define (memo2 init)
  (let ([results (make-hash)])
    (lambda (arg1 arg2)
      (hash-ref! results
                 (list arg1 arg2)
                 (lambda () (init arg1 arg2))))))

;; Use this thunk to find the set of paths we're benchmarking.
(define theorem-files
  (let ()
    ;; By default, use all  files in benchmark-dir.
    (memo0 all-theorem-files
           (sort (map path->string
                      (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
                              (sequence->list (in-directory benchmark-dir))))
                 string<=?))
    all-theorem-files))

;; Override the theorem files to be used. If you're going to use this, do it
;; before computing anything, to prevent stale values being memoised. Basically
;; only exists for making the test suite quicker.
;;
;; REMEMBER: theorem-files should be a thunk returning a list, not a list!
(define (set-theorem-files! proc)
  (set! theorem-files (lambda ()
                        (sort (proc) string<=?))))

(define (symbols-of-theorem path)
  (benchmark-symbols (file->list path)))

;; Reads a list of expressions from the given string
(define (read-benchmark x)
  (let* ([content (string-append "(\n" x "\n)")])
    (with-input-from-string content
      read)))

;; Reads a list of expressions from the given file
(define (file->list f)
  (read-benchmark (file->string f)))

;; Return the names of all functions defined in the given expr, including
;; destructors; i.e. those things which need lowercase initials in Haskell.
(define (lowercase-names expr)
  (match expr
    [(list 'define-fun-rec (list 'par _ (list name _ _ _))) (list name)]
    [(list 'define-fun-rec name _ _ _)                      (list name)]
    [(list 'define-fun (list 'par _ (list name _ _ _)))     (list name)]
    [(list 'define-fun name _ _ _)                          (list name)]
    [(list 'define-funs-rec decs _)
     (map (lambda (dec)
            (if (equal? (first dec) 'par)
                (first (third dec))
                (first dec)))
          decs)]

    [(list 'declare-datatypes _ decs)
     (concat-map (lambda (dec)
                   (define constructor-decs
                     (cdr dec))
                   (define destructor-decs
                     (concat-map cdr constructor-decs))
                   (map first destructor-decs))
                 decs)]
    [(cons a b) (append (lowercase-names a) (lowercase-names b))]
    [_          '()]))

;; Return the names of all types and constructors defined in the given expr;
;; i.e. those things which need uppercase initials in Haskell.
(define (uppercase-names expr)
  (match expr
    [(list 'declare-datatypes _ type-decs)
     (append
      ;; Type names
      (map first type-decs)
      ;; Constructor names
      (concat-map (lambda (type-dec)
                   (define constructor-decs
                     (cdr type-dec))
                   (map first constructor-decs))
                 type-decs))]

    [(cons a b) (append (uppercase-names a) (uppercase-names b))]
    [_          '()]))

;; Symbols used in EXPR. TODO: redundant?
(define (benchmark-symbols expr)
  (remove-duplicates (expression-symbols expr)))

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
          [           (list name        args        return body)
                      (let* ([fun (norm-func args body)])
                        (list ps (list norm-func-1 (first fun) return (replace-in name
                                                                                  norm-func-1
                                                                                  (second fun)))))]
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

;; Returns all names defined in DEFS
(define (names-in defs)
  (append (lowercase-names defs)
          (uppercase-names defs)))

;; Rename constructors defined in X to begin with "constructor-"
(define (tag-constructors x)
  ;; Tag constructors with 'constructor-' to disambiguate
  (foldl (lambda (c y)
           (replace-in c (prefix-name c "constructor-") y))
         x
         (expression-constructors x)))

;; Rename types in X to begin with "type-"
(define (tag-types x)
  ;; Tag types with 'type-' to disambiguate
  (foldl (lambda (t y)
           (if (and (symbol? t)
                    (not (member t native-symbols)))
               (replace-in t (prefix-name t "type-") y)
               y))
         x
         (expression-types x)))

;; Prefix symbol N with string P
(define (prefix-name n p)
  (string->symbol (string-append p (symbol->string n))))

;; Apply F to each element of XS, and append the results together
(define (concat-map f xs)
  (apply append (map f xs)))

;; Backported from Racket 6.7
(define index-where
  (let ()
    (define (index-where-acc acc lst proc)
      (if (empty? lst)
          #f
          (if (proc (car lst))
              acc
              (index-where-acc (+ 1 acc) (cdr lst) proc))))
    (lambda (lst proc)
      (index-where-acc 0 lst proc))))

;; Remove TIP boilerplate
(define (trim lst)
  (filter (lambda (x)
            (and (not (equal? (first x) 'assert-not))
                 (not (equal? x '(check-sat)))))
          lst))

;; Returns the last N elements of LST
(define (take-from-end n lst)
  (reverse (take (reverse lst) n)))

;; The last part of a path, which is enough to distinguish a TIP benchmark
(define (path-end s)
  (string-join (take-from-end 2 (string-split s "/")) "/"))

;; Read all files named in GIVEN-FILES, combine their definitions together and
;; prefix each name with the path of the file it came from
(define qual-all
  (memo1 (lambda (given-files)
  (define given-contents
    (map (lambda (pth)
           (list (path-end pth) (file->list pth)))
         given-files))

  (define qualified-contents
    (map (lambda (name-content)
           (qualify (first name-content) (second name-content)))
         given-contents))

  (trim (apply append qualified-contents)))))

;; Apply mk-defs to stdio
(define (mk-defs)
  (show (mk-defs-s (port->lines (current-input-port)))))

;; Read all files named in GIVEN-FILES, combine their definitions together and
;; remove alpha-equivalent duplicates
(define (mk-defs-s given-files)
  (norm-defs (qual-all given-files)))

(define (non-empty? x)
  (not (empty? x)))

;; Equality which allows symbols and strings
(define (ss-eq? x y)
  (cond ([symbol? x]
         (ss-eq? (symbol->string x) y))
        ([symbol? y]
         (ss-eq?  x (symbol->string y)))
        (#t
         (equal? x y))))

;; Return any definitions of function F which appear in X
(define (find-sub-exprs f x)
  (match x
    [(list 'define-fun name _ _ _)                          (if (ss-eq? name f)
                                                                (list x)
                                                                '())]
    [(list 'define-fun (list 'par _ (list name _ _ _)))     (if (ss-eq? name f)
                                                                (list x)
                                                                '())]
    [(list 'define-fun-rec   name _ _ _)                    (if (ss-eq? name f)
                                                                (list x)
                                                                '())]
    [(list 'define-fun-rec (list 'par _ (list name _ _ _))) (if (ss-eq? name f)
                                                                (list x)
                                                                '())]
    [(list 'define-funs-rec _ _)       (if (member f (names-in x) ss-eq?)
                                           (list x)
                                           '())]
    [(cons h t)                        (let ([in-h (find-sub-exprs f h)])
                                         (if (empty? in-h)
                                             (find-sub-exprs f t)
                                             in-h))]
    [_                                 '()                              ]))

;; Idempotent symbol->string
(define (as-str x)
  (if (string? x)
      x
      (symbol->string x)))

;; Returns TRUE if any element of XS passes predicate F, FALSE otherwise
(define (any-of f xs)
  (match xs
    [(cons a b) (or (f a) (any-of f b))]
    [_          #f]))

;; Returns FALSE if any element of XS fails predicate F, TRUE otherwise
(define (all-of f xs)
  (match xs
    [(cons a b) (and (f a) (all-of f b))]
    [_          #t]))

;; Return any definitions of NAME appearing in EXPRS, where NAME can be of a
;; function, type, constructor or destructor
(define/test-contract (get-def-s name exprs)
  (-> symbol? any/c any/c)

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

  (remove-duplicates (append (find-sub-exprs name (list exprs))
                             (concat-map defs-from (list exprs)))))

;; Applies get-def to a sting of definitions; TODO: remove?
(define (get-def name str)
  (get-def-s name (read-benchmark str)))

;; Strip "-sentinel" once we've finished processing names
(define (remove-suffix x)
  (string->symbol
   (string-reverse
    (substring (string-reverse (symbol->string x))
               9))))

;; Removes '-sentinel' suffices. Do this after all other string-based
;; transformations, since the sentinels prevent us messing with, say, the
;; symbol "plus2", when we only wanted to change the symbol "plus". Also
;; unqualifies "custom-foo" definitions, e.g. custom->, etc.
(define (unqualify x)
  ;; Removes any prefix from occurrences of the given name appearing in the
  ;; given expression
  (define (unqual name expr)
    (match expr
      [(? symbol?) (if (string-suffix? (symbol->string expr)
                                       (symbol->string name))
                       name
                       expr)]
      [(cons y ys) (cons (unqual name y) (unqual name ys))]
      [_           expr]))

  (foldl unqual
         (read-benchmark (string-replace (format-symbols x)
                                         "-sentinel"
                                         ""))
         '(CustomBool CustomTrue CustomFalse
           custom-ite custom-not custom-and custom-or custom-=>
           custom-bool-converter
           CustomNat CustomZ CustomS custom-p custom-plus
           CustomInt CustomNeg custom-succ CustomZero CustomPos custom-pred
           custom-inc custom-dec custom-invert custom-abs custom-sign custom-+
           custom-- custom-* custom-nat-> custom-> custom-div custom-mod
           custom-< custom->= custom-<=)))

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

        (concat-map arg-decs-for-con (cdr x)))

      (match definition
        [(list 'declare-datatypes _ decs) (concat-map arg-decs-for-ty decs)]))

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

;; Convert STR to a hex encoding of its ASCII bytes
(define (encode16 str)
  (define chars
    (string->list str))

  (define codepoints
    (map char->integer chars))

  (define rawhex
    (map (lambda (p)
           (number->string p 16))
         codepoints))

  (define padded
    (map (lambda (s)
           (string-reverse
            (substring (string-reverse (string-append "0" s))
                       0
                       2)))
         rawhex))
  (apply string-append padded))

;; Interpret STR as a hex encoding of ASCII bytes, returning the decoded content
(define (decode16 str)
  (define hex-pairs
    (letrec ([get-pairs (lambda (s)
                          (if (non-empty-string? s)
                              (cons (substring s 0 2)
                                    (get-pairs (substring s 2 (string-length s))))
                              '()))])
      (get-pairs str)))

  (define codepoints
    (map (lambda (pair)
           (string->number pair 16))
         hex-pairs))

  (define chars
    (map integer->char codepoints))

  (list->string chars))

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

;; Encode a function name for surviving Haskell translation
(define (encode-lower-name name)
  (string->symbol (string-append "global"
                                 (encode16 (symbol->string name)))))

;; Encode a type or constructor name for surviving Haskell translation
(define (encode-upper-name name)
  (string->symbol (string-append "Global"
                                 (encode16 (symbol->string name)))))

(define global-length
  (string-length "global"))

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
              (substring s (+ start global-length) end))

            (define name
              (decode16 encoded-name))

            (define new-diff
              (- end start (string-length name)))

            (list (string-append prefix name suffix) (- diff new-diff)))
          (list s 0)
          (regexp-match-positions* #rx"[Gg]lobal[0-9a-f]+" s))))

;; Turns a list of definitions X into a form suitable for sending into the tip
;; tools
(define (prepare x)
  (define (add-check-sat x)
    ;; Add '(check-sat)' as the last line to appease tip-tools
    (append x '((check-sat))))

  (add-check-sat
   (encode-names
    (preprepare x))))

(define (preprepare x)
  (add-constructor-funcs
   (add-destructor-funcs
    (unqualify x))))

;; Creates a list of pairs '((X1 Y1) (X2 Y2) ...) when given a pair of lists
;; '(X1 X2 ...) and '(Y1 Y2 ...)
(define (zip xs ys)
  (if (empty? xs)
      null
      (if (empty? ys)
          null
          (cons (list (car xs) (car ys))
                (zip  (cdr xs) (cdr ys))))))

;; Compare symbol names lexicographically
(define (symbol<=? x y)
  (string<=? (symbol->string x)
             (symbol->string y)))

;; Compare symbol names lexicographically
(define (symbol<? x y)
  (string<? (symbol->string x)
            (symbol->string y)))

;; Compare values, lexicographically on their displayed output
(define (arbitrary<=? x y)
  (string<=? (~a x) (~a y)))

;; Predicate for whether X defines any TIP type/function/etc.
(define (definition? x)
  (and (not (empty? (names-in x)))
       (not (any-of (lambda (sub-expr)
                      (not (empty? (names-in sub-expr))))
                    x))))

;; Looks for alpha-equivalent definitions in RAW-EXPRS, and returns a list of
;; name replacements '((OLD1 NEW1) (OLD2 NEW2) ...) which can be used to update
;; references and remove redundancies. Each NEW name is the smallest,
;; lexicographically, which makes subsequent comparisons easier.
(define/test-contract (find-redundancies raw-exprs)
  (-> (and/c (*list/c definition?)
             (lambda (exprs)
               ;; Make sure we're not given encoded names, since their
               ;; lexicographic order will differ from the unencoded versions.
               ;; Strictly speaking, we should allow such names in case the user
               ;; actually fed in such definitions; however, in practice this
               ;; is a good indicator that something's wrong in our logic!
               (all-of (lambda (name)
                         (not (regexp-match? "[Gg]lobal[0-9a-f]+"
                                             (symbol->string name))))
                       (names-in exprs)))
             (lambda (exprs)
               ;; Like above, but make sure we're not given normalised names
               (all-of (lambda (name)
                         (not (regexp-match? "normalise-var-[0-9]"
                                             (symbol->string name))))
                       (names-in exprs))))
      (*list/c (list/c symbol? symbol?)))

  (define exprs
    (remove-duplicates raw-exprs))

  (define normalised-def?
    (and/c definition?
           (lambda (expr)
             (all-of (lambda (name)
                       (or (regexp-match? "^normalise-"
                                          (symbol->string name))
                           (regexp-match? "^defining-"
                                          (symbol->string name))))
                     (names-in expr)))))

  (define class?
    (and/c (*list/c (*list/c symbol?))
           (lambda (class)
             ;; Each definition should give the same number of names, or else
             ;; there's no way they'd be alpha equivalent
             (equal? 1
                     (length (remove-duplicates (map length class)))))))

  (define (mk-output expr so-far)
    (let* ([norm-line (norm     expr)]
           [names     (names-in expr)]
           ;; Look for an existing alpha-equivalent definition
           ;;   '(((name1 name2 ...) expr) ...)
           [existing-pos (index-where so-far (lambda (x)
                                               (equal? norm-line (second x))))])
      (if (equal? #f existing-pos)
          ;; This expr isn't redundant, associate its names with its normal form
          (append so-far (list (list (list names) norm-line)))

          ;; This expr is redundant, include its names in the replacement list
          (list-update so-far existing-pos (lambda (existing)
                                             (list (cons names (first existing))
                                                   norm-line))))))

  (define (remove-redundancies exprs so-far)
    (if (empty? exprs)
        so-far
        (remove-redundancies (cdr exprs)
                             (mk-output (first exprs) so-far))))

  (define so-far (remove-redundancies exprs null))

  ;; Choose the smallest name out of the alternatives found in exprs.
  ;;
  ;; Example input: '(((Pair mkPair first second)
  ;;                   (PairOf paired fst snd))
  ;;                  (declare-datatypes (normalise-var-2 normalise-var-1)
  ;;                     ((defining-type-1
  ;;                        (normalise-constructor-1
  ;;                          (normalise-destructor-2 normalise-var-2)
  ;;                          (normalise-destroctor-1 normalise-var-1)))))
  ;;
  ;; Output (old new) pairs to replace larger names with smaller, e.g. in the
  ;; above example we'd get '((PairOf Pair)
  ;;                          (paired mkPair)
  ;;                          (fst    first)
  ;;                          (snd    second))

  ;; Pick the lexicographically-smallest names as the replacements
  (define all-classes
    (map first so-far))

  (define (pick-replacements class)
    (if (empty? class)
        ;; Nothing to replace
        '()
        (if (empty? (first class))
            ;; We've plucked all of the names out of this class
            '()
            (let*
                ;; Pluck the first names from all sets in this class
                ([these (sort (map first class) symbol<=?)]

                 ;; Pluck out the smallest, which will be the canonical name
                 [new   (first these)]

                 ;; Define replacements for all non-canonical names
                 [replacements (map (lambda (old) (list old new))
                                    (cdr these))])

              ;; Recurse, dropping the names we just processed
              (append replacements (pick-replacements (map cdr class)))))))

  ;; Make list of replacements, based on smallest element of each class
  (concat-map pick-replacements all-classes))

;; Is X a permutation of Y?
(define (set-equal? x y)
  (equal? (list->set x) (list->set y)))

;; TODO: Clean up
(define (symbols-of-theorems-s expr)
  (filter (lambda (s)
            (not (member s '(true-sentinel
                             false-sentinel
                             or-sentinel
                             ite-sentinel))))
          (benchmark-symbols expr)))

;; TODO: Clean up
(define (symbols-of-theorems)
  (displayln (string-join (map ~a (symbols-of-theorems-s
                                   (read-benchmark
                                    (port->string (current-input-port)))))
                          "\n")))

;; Run F with the string S as its input port. Returns whatever F writes to its
;; output port.
(define (pipe s f)
  (define o (open-output-string))
  (parameterize ([current-input-port  (open-input-string s)]
                 [current-output-port o])
    (f))
  (get-output-string o))

;; TODO: Do we need this?
(define (qualify-given)
  (define given
    (read-benchmark (port->string (current-input-port))))

  (define name
    (string-replace (getenv "NAME") "'" "_tick_"))

  (show (qualify name given)))

;; For each (SRC DST) in REPS, replaces SRC with DST in STR
(define (replace-strings str reps)
  (foldl (lambda (pair so-far)
           (string-replace so-far (as-str (first  pair))
                                  (as-str (second pair))))
         str
         reps))

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
(define (norm-defs exprs)
  (first (normed-and-replacements exprs)))

;; Given a list of definitions, returns the name of those which are
;; alpha-equivalent '((DUPE1 CANON1) (DUPE2 CANON2) ...), where each DUPEi is
;; the name of a definition that's equivalent to the definition named by CANONi.
;;
;; When we find a set of duplicate definitions, we choose the lexicographically-
;; smallest name to be the canonical one.
;;
;; Note that we take the *transitive closure* when equivalence-checking: for
;; each equivalent pair A and B that we find, we check for equivalence *given
;; that A and B are equivalent*, and so on recursively, until we can't find any
;; more.
(define (replacements-closure exprs)
  (second (normed-and-replacements exprs)))

;; Removes redundant alpha-equivalent definitions from EXPRS, resulting in a
;; normalised form given as the first element of the result.
;; Also keeps track of the replacements it's made in the process, returning them
;; as the second element of the result.
(define normed-and-replacements
  ;; All of the hard work is done inside normed-and-replacements-inner, but that
  ;; function is recursive, so memoising it would fill the lookup table with
  ;; intermediate results.
  (memo1 (lambda (exprs)
           (normed-and-replacements-inner exprs '()))))

(define/test-contract (normed-and-replacements-inner exprs reps)
  (-> (*list/c definition?)
      (*list/c (list/c symbol? symbol?))
      (list/c (*list/c definition?) (*list/c (list/c symbol? symbol?))))

  (log "Normalising ~a definitions\n" (length exprs))

  ;; Find the names of redundant definitions, and a canonical replacement
  (define/test-contract redundancies
    (and/c (*list/c (and/c (list/c symbol? symbol?)
                           (lambda (pair)
                             (symbol<? (second pair) (first pair)))))
           (lambda (redundancies)
             (all-of (lambda (pair)
                       (or (not (member (first pair)
                                        (map second redundancies)))
                           (raise-user-error
                            'redundancies
                            "Redundant name '~a', with canonical replacement '~a', shouldn't appear in the canonical names:\n~a"
                            (first  pair)
                            (second pair)
                            (map second redundancies))))
                     redundancies)))

    (find-redundancies exprs))

  (log "Found ~a redundancies\n" (length redundancies))

  ;; Switch out all of the redundant names (including in definitions)
  (define/test-contract renamed
    (and/c (*list/c (and/c definition?
                           (lambda (expr)
                             (not (any-of (lambda (name)
                                            (member name (map first redundancies)))
                                          (names-in expr))))))
           (flat-contract-with-explanation
            (lambda (renamed)
              (or (all-of (lambda (name)
                            (member name (names-in renamed)))
                          (map second redundancies))
                  (lambda (blame)
                    (raise-blame-error
                     blame renamed
                     (list 'expected: "definitions including canonical names"
                           'given:    "definitions missing canonical names"
                           "Should have definitions for at least ~a"
                           "Only found definitions for ~a")
                     (map second redundancies)
                     (names-in renamed))))))
           (flat-contract-with-explanation
            (lambda (renamed)
              (or (equal? (length renamed)
                          (length exprs))
                  (lambda (blame)
                    (raise-blame-error
                     blame renamed
                     (list 'expected: "~a definitions"
                           'given:    "~a definitions")
                     (length exprs)
                     (length renamed)))))))

    (replace-all redundancies exprs))

  (log "Renamed\n")

  (define/test-contract (strip-acc expr seen-result)
    (-> definition? (list/c (*list/c symbol?) (*list/c definition?))
        (list/c (*list/c symbol?) (*list/c definition?)))

    (let* ([seen      (first  seen-result)]
           [result    (second seen-result)]
           [def-names (names-in expr)])
      (if (all-of (lambda (name)
                    (member name seen))
                  def-names)
          (list seen result)
          (list (append seen def-names)
                (append result (list expr))))))

  (define/test-contract stripped
    (and/c (*list/c definition?)
           (lambda (stripped)
             (equal? stripped (remove-duplicates stripped)))
           (lambda (stripped)
             (all-of (lambda (name)
                       (member name (names-in stripped)))
                     (map second redundancies)))
           (lambda (stripped)
             (not (any-of (lambda (name)
                            (member name (map first redundancies)))
                          (names-in stripped)))))

    (second (foldl strip-acc
                   (list '() '())
                   renamed)))

  (log "Stripped ~a definitions containing redundancies\n"
       (- (length exprs) (length stripped)))

  (define/test-contract replacement-closure
    (and/c (*list/c (list/c symbol? symbol?))

           (lambda (replacement-closure)
             (all-of (lambda (pair1)
                       (all-of (lambda (pair2)
                                 (when (equal? (first  pair1)
                                               (second pair2))
                                   (error "'old' entry also appears as 'new'"
                                          'pair1 pair1
                                          'pair2 pair2))
                                 #t)
                               replacement-closure))
                     replacement-closure))

           (lambda (replacement-closure)
             (all-of (lambda (pair)
                       (or (symbol<? (second pair) (first pair))
                           (error "Replacement not canonical"
                                  'pair pair)))
                     replacement-closure)))

    (foldl (lambda (rep existing)
             (cons rep
                   (map (lambda (pair)
                          ;; If rep = '(old new) and pair = '(ancient old),
                          ;; replace pair with '(ancient new)
                          (if (equal? (first rep) (second pair))
                              (list (first pair) (second rep))
                              pair))
                        existing)))
           reps
           redundancies))

  (if (equal? exprs stripped)
      (list                          stripped replacement-closure)
      (normed-and-replacements-inner stripped replacement-closure)))

(define (defs-to-sig x)
  (mk-signature-s (format-symbols (mk-final-defs-s (string-split x "\n")))))

(define (mk-final-defs)
  (show (mk-final-defs-s (port->lines (current-input-port)))))

;; Read in the files names in GIVEN-FILES and return a combined, normalised TIP
;; benchmark
(define (mk-final-defs-s given-files)
  (prepare (mk-defs-s given-files)))

;; Prefix used to identify temporary files as ours
(define temp-file-prefix
  "tebenchmarktemp")

;; Debug dump to stderr
(define (dump x)
  (write x (current-error-port))
  x)

;; Sends a string INPUT through the `tip` program for conversion to Haskell +
;; QuickSpec
(define (mk-signature-s input)
  (run-pipeline/out `(echo ,input)
                    '(tip --haskell-spec)))

;; A wrapper around `tip`
(define (mk-signature)
  (display (mk-signature-s (port->string (current-input-port)))))

;; Run BODY with VARS present in the environment variables
(define (parameterize-env vars body)
  (let* ([old-env (environment-variables-copy (current-environment-variables))]
         [new-env (foldl (lambda (nv env)
                           (environment-variables-set! env (first  nv)
                                                           (second nv))
                           env)
                         old-env
                        vars)])
    (parameterize ([current-environment-variables new-env])
      (body))))

(define (types-from-defs)
  (show (symbols-in
         (remove-duplicates
          (symbols-in
           (expression-types
            (read-benchmark (port->string (current-input-port)))))))))

;; Create a Haskell package in directory DIR containing rendered benchmark
;; definition STR
(define (full-haskell-package-s str dir)
  (define hs (mk-signature-s str))

  ;; Remove the generated signature, as it's incompatible with QuickSpec 1, and
  ;; remove the import of QuickSpec 2
  (define patched
    (filter (lambda (line)
              (not (string-contains? line "import qualified QuickSpec as QS")))
            (takef (string-split hs "\n")
                   (lambda (line)
                     (not (string-prefix? line "sig ="))))))

  (make-directory (string-append dir "/src"))

  (display-to-file (string-join patched "\n")
                   (string-append dir "/src/A.hs"))
  (display-to-file "-- Initial tip-benchmark-sig.cabal generated by cabal init.  For further
-- documentation, see http://haskell.org/cabal/users-guide/

name:                tip-benchmark-sig
version:             0.1.0.0
synopsis:            Auto-generated package for theory exploration
-- description:
homepage:            http://example.org
license:             PublicDomain
license-file:        LICENSE
author:              Chris Warburton
maintainer:          chriswarbo@gmail.com
-- copyright:
category:            Testing
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10

library
  exposed-modules:     A
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.8
                     , quickspec
                     , QuickCheck
                     , testing-feat
  hs-source-dirs:      src
  default-language:    Haskell2010
"
                   (string-append dir "/tip-benchmark-sig.cabal"))

  (display-to-file "Auto-generated from https://github.com/tip-org/benchmarks, the same LICENSE applies"
                   (string-append dir "/LICENSE")))

;; Commandline wrapper around full-haskell-package-s using stdio and env vars
(define (full-haskell-package)
  (full-haskell-package-s (port->string (current-input-port))
                          (getenv "OUT_DIR")))

;; Extracts a list of theorem statements from a given benchmark file. For real
;; TIP benchmarks this should contain a single theorem, so we enforce this in
;; the contract. Many of our generated files (e.g. used for translating) do not
;; contain any theorems, and hence it makes no sense to pass them into this
;; function. There should never be multiple theorems.
(define/test-contract (theorems-from-file f)
  (-> any/c (lambda (result)
              (or (and (list? result)
                       (equal? (length result) 1))
                  (raise-user-error
                   'result
                   "Expected a single (negated) theorem. Found ~a"
                   result))))

  (define (get-theorems x)
    (match x
      [(list 'assert-not _) (list x)]
      [(cons h t)           (append (get-theorems h) (get-theorems t))]
      [_                    '()]))

  (get-theorems (file->list f)))

(memo0 benchmark-theorems
       (make-immutable-hash
        (foldl (lambda (f rest)
                 (cons (cons f (first (theorems-from-file f)))
                       rest))
               '()
               (theorem-files))))

(define (theorem-of f)
  (hash-ref (benchmark-theorems) f
            (lambda ()
              (raise-arguments-error
               'theorem-of
               "No theorem found"
               "given-file" f
               "benchmark-dir (from BENCHMARKS)" benchmark-dir
               "benchmark-theorems" (benchmark-theorems)))))

(define (theorem-globals thm)
  (define (thm-locals expr)
    (match expr
      [(list 'assert-not x)     (thm-locals x)]
      [(list 'par params body)  (append params (thm-locals body))]
      [(list 'forall vars body) (append (map first vars)
                                        (thm-locals body))]
      [(cons x y)               (append (thm-locals x) (thm-locals y))]
      [_                        '()]))

  (define (thm-names expr)
    (match expr
      [(list 'assert-not x)     (thm-names x)]
      [(list 'forall vars body) (append (concat-map (lambda (var)
                                                      (symbols-in (second var)))
                                                    vars)
                                        (symbols-in body))]
      [(list 'par _ body)       (thm-names body)]
      [(cons x y)               (append (thm-names x) (thm-names y))]
      [_                        (symbols-in expr)]))


    (remove* (thm-locals thm) (thm-names thm)))

(define (qual-thm filename thm)
  (replace-all (map (lambda (g)
                      (list g
                            (string->symbol (string-append (path-end filename)
                                                           (symbol->string g)
                                                           "-sentinel"))))
                    (theorem-globals thm))
               thm))

(memo0 normalised-theorems
       (define qualified
         (qual-all (theorem-files)))

       ;; First get replacements used in definitions
       (define replacements
         (replacements-closure qualified))

       ;; Also replace constructors with constructor functions, skipping
       ;; constructors which are redundant

       (define all-constructors
         (expression-constructors qualified))

       (define constructor-replacements
         (map (lambda (c)
                (list c (prefix-name c "constructor-")))
              (remove* (map first replacements)
                       all-constructors)))

       ;; Update replacements to use constructor functions rather than
       ;; constructors
       (define final-replacements
         (append constructor-replacements
                 (map (lambda (rep)
                        (if (member (second rep)
                                    (map first constructor-replacements))
                            (list (first rep) (prefix-name (second rep)
                                                           "constructor-"))
                            rep))
                      replacements)))

       (make-immutable-hash
        (hash-map (benchmark-theorems)
                  (lambda (f thm)
                    (cons f (unqualify
                             (replace-all final-replacements
                                          (qual-thm f thm))))))))

(define (normed-theorem-of f)
  (hash-ref (normalised-theorems) f
            (lambda ()
              (raise-arguments-error
               'normed-theorem-of
               "No theorem found"
               "given-file" f))))

(define (theorem-types expr)
  (match expr
    [(list 'forall vars body) (append (theorem-types body)
                                      (concat-map (lambda (var)
                                                    (symbols-in (second var)))
                                                  vars))]
    [(list 'as x t)           (append (theorem-types x)
                                      (symbols-in t))]
    [(list 'lambda vars body) (append (theorem-types body)
                                      (concat-map (lambda (var)
                                                    (symbols-in (second var)))
                                                  vars))]
    [(cons x y)               (append (theorem-types x) (theorem-types y))]
    [_                        '()]))

(memo0 normed-qualified-theorem-files
       (preprepare (norm-defs (qual-all (theorem-files)))))

(define theorem-deps-of
  (memo1 (lambda (f)
           (log "Forcing theorem deps of ~a\n" f)
           (define normed (normed-theorem-of f))

           ;; Includes all (canonical) types and constructors
           (define uppercase (uppercase-names (normed-qualified-theorem-files)))

           (define constructors
             (expression-constructors (normed-qualified-theorem-files)))

           ;; Remove types
           (define raw-names
             (remove* (theorem-types normed) (theorem-globals normed)))

           ;; Remove builtins
           (define custom-names
             (remove* native-symbols raw-names))

           (define result
             (foldl (lambda (name existing)
                      ;; Prefix constructors, so we use the function instead
                      (cons (if (member name constructors)
                                (prefix-name name "constructor-")
                                name)
                            existing))
                    '()
                    raw-names))
           (log "Finished theorem deps of ~a\n" f)
           result)))

(memo0 all-theorem-deps
       (map (lambda (f) (list f (list->set (theorem-deps-of f))))
            (theorem-files)))

;; Does S contain all dependencies required by some theorem statement?
(define (sample-admits-conjecture? s)
  (any-of (lambda (f-deps)
            (subset? (second f-deps) s))
          (all-theorem-deps)))

;; Deterministically, but unpredictably, shuffle the given NAMES. KEYGEN turns
;; a name into a hash, and we perform the shuffle by sorting the hashes.
(define (deterministic-shuffle keygen names)
  (define sorted
    (sort (map (lambda (n) (list n (keygen n)))
               names)
          (lambda (x y)
            (not (bytes>? (second x) (second y))))))
  (map first sorted))

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

  ;; To avoid division by zero in precision/recall experiments, we only return
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

  (log "Converted all theorem dependencies into constraints\n")

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
  (define lcm
    (lambda args
      (match args
        [(list x)             x]
        [(cons x (cons y zs)) (apply lcm (cons (* x (/ y (gcd x y)))
                                               zs))])))

  (define constraint-lcm
    (apply lcm (map set-count constraints)))

  ;; Now we calculate the relative chances of each constraint appearing in a
  ;; uniform sample, by just performing these divisions.
  (define constraint-freqs
    (map (lambda (c)
           (list c (/ constraint-lcm (set-count c))))
         constraints))

  (log "Calculated frequency for each constraint\n")

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

  (log "Shuffling names\n")

  ;; Now we need to pad our constraint with uniformly chosen names. To do this,
  ;; we shuffle all of the names and choose from the start of the list.
  (define shuffled
    (deterministic-shuffle get-hash names))

  (define padding
    (take (remove* (set->list chosen-constraint) shuffled)
          (- size (set-count chosen-constraint))))

  (log "Obtained sample\n")

  (list->set (append (set->list chosen-constraint) padding)))

;; Normalised benchmark from given BENCHMARKS
(memo0 final-benchmark-defs
       (mk-final-defs-s (theorem-files)))

;; All function names defined in given BENCHMARKS. NOTE: These will be
;; hex-encoded.
(memo0 lowercase-benchmark-names
       (lowercase-names (final-benchmark-defs)))

(define/test-contract (bytes->hex bs)
  (-> bytes?
      (lambda (result)
        (unless (string? result)
          (raise-user-error
           'bytes-to-hex
           "Should have made a string, actually made ~a" result))
        (for-each (lambda (char)
                    (unless (member char (string->list "0123456789abcdef"))
                      (raise-user-error
                       'bytes-to-hex
                       "Should have outputted hex characters, gave ~a" result)))
                  (string->list result))))
  (foldl (lambda (byte rest)
           (string-append (number->string byte 16) rest))
         ""
         (bytes->list bs)))

;; Cache data required for sampling in /tmp, so we can draw samples over and
;; over from the same benchmarks without recalculating everything each time.

(define ((assoc-contains? . keys) l)
  (unless (list? l)
    (raise-user-error
     'assoc-contains
     "Expected a list, given ~s" l))
  (all-of (lambda (key)
            (or (any-of (lambda (pair)
                          (and (pair? pair)
                               (equal? (car pair) key)))
                        l)
                (raise-user-error
                 'assoc-contains
                 "Couldn't find entry for ~s in ~s" key l)))
          keys))

(define sampling-data?
  (assoc-contains? 'all-canonical-function-names
                   'theorem-deps
                   'equation-names))

(define/test-contract (get-sampling-data)
  (-> sampling-data?)

  ;; Check if we have cached data, if so then we return that. If not, we run
  ;; the thunk MK-DATA, cache the result to disk, then return the result.

  (define/test-contract (mk-data)
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

      (equation-names
       ,(filter (lambda (f)
                  (not (empty? (equation-from f))))
                (theorem-files)))))

  ;; The path where we'll cache data. To avoid runaway file deletion, we include
  ;; both "/tmp" and the disambiguating prefix in a single hard-coded string.
  ;; This guarantees that concatenating with dodgy variables, like "", will
  ;; never try to delete important paths like "/" or "/tmp".
  (define path-prefix "/tmp/tebenchmarktemp-cache-")

  ;; If you change the format of the data being stored, bump the version number
  ;;to avoid being given the old format
  (define cache-path
    (string-append path-prefix
                   (bytes->hex (sha256 (~a `(version-2 ,(theorem-files)))))))

  ;; Check if cached data exists for these parameters
  (define (have-cached-data?)
    (file-exists? cache-path))

  (unless (have-cached-data?)
    ;; Don't create an unbounded number of files; delete if there are too many
    (define existing
      (filter (lambda (name)
                (string-prefix? (string-append "/tmp/" name)
                                path-prefix))
              (map some-system-path->string (directory-list "/tmp"))))

    (when (> (length existing) 1000)
      (log "Cache too big, deleting some files matching ~a*\n" path-prefix)
      (for-each (lambda (name)
                  (define suffix
                    (substring (some-system-path->string name)
                               (- (length path-prefix) (length "/tmp/"))))
                  (delete-directory/files
                   (string-append path-prefix suffix)))
                (take existing 100)))

    (log "No cached data found, calculating from scratch\n")
    (define out
      (open-output-file cache-path #:exists 'replace))
    (write (mk-data) out)
    (close-output-port out)

    (unless (have-cached-data?)
      (raise-user-error
       'cache-to-disk
       "Sanity check failed: couldn't find cache after writing it ~s"
       cache-path)))

  (define in   (open-input-file cache-path))
  (define data (read in))
  (close-input-port in)

  data)

(define (assoc-get key val)
  (second (assoc key val)))

;; Sample using the names and theorems from BENCHMARKS
(define (sample-from-benchmarks size rep)
  (define data (get-sampling-data))

  (define-values (all-canonical-function-names theorem-deps)
    (values (assoc-get 'all-canonical-function-names data)
            (map (lambda (t-d)
                   (list->set (second t-d)))
                 (assoc-get 'theorem-deps data))))

  (define sampled
    (sample size rep all-canonical-function-names theorem-deps))

  ;; Hex encode sample so it's usable with e.g. Haskell translation
  (map-set encode-lower-name sampled))

;; Sample using the names and equational theorems from BENCHMARKS
(define (sample-equational-from-benchmarks size rep)
  (define data (get-sampling-data))

  (define-values (all-canonical-function-names theorem-deps equation-names)
    (values                (assoc-get 'all-canonical-function-names data)
            (map list->set (assoc-get 'theorem-deps                 data))
                           (assoc-get 'equation-names               data)))

  ;; Throw away (theorem dependencies) pairs if theorem isn't in equation-names,
  ;; then turn the resulting pairs into sets of dependencies.
  (define equation-deps
    (map (lambda (t-d)
           (list->set (second t-d)))
         (filter (lambda (t-d)
                   (member (first t-d) equation-names))
                 theorem-deps)))

  (sample size rep all-canonical-function-names equation-deps))

;; Map a function F over the elements of a set S
(define (map-set f s)
  (list->set (set-map s f)))

(define/test-contract (precision found wanted)
  (-> set? set? rational?)
  (/ (set-count (set-intersect found wanted))
     (set-count found)))

(define/test-contract (recall found wanted)
  (-> set? set? rational?)
  (/ (set-count (set-intersect wanted found))
     (set-count wanted)))

(define (find-eqs-intersection found sample)
  (define possibilities
    (conjectures-admitted-by sample))

  (define eqs
    (concat-map theorem-to-equation possibilities))

  (define intersection
    (foldl (lambda (eq result)
             (if (any-of (lambda (a-found)
                           (equations-match? a-found eq))
                         found)
                 (cons eq result)
                 result))
           '()
           eqs))

  (list possibilities eqs intersection))

(define (precision-from-sample found sample)
  (match (find-eqs-intersection found sample)
    [(list possibilities eqs intersection)
     (/ (length intersection) (length found))]))

;; We only find equations so precision for equations is the same as for theorems
(define precision-eqs-from-sample precision-from-sample)

(define (recall-from-sample found sample)
  (match (find-eqs-intersection found sample)
    [(list possibilities eqs intersection)
     (/ (length intersection) (length possibilities))]))

(define (recall-eqs-from-sample found sample)
  (match (find-eqs-intersection found sample)
    [(list possibilities eqs intersection)
     (/ (length intersection) (length possibilities))]))

;; Return all theorems (expressions) which would be possible to discover given
;; what's in the provided sample. In other words, those theorems whose
;; dependencies are a subset of the sample.
(define (conjectures-admitted-by sample)
  (define theorem-deps
    (assoc-get 'theorem-deps (get-sampling-data)))

  (define theorem-files-admitted
    (map first
         (filter (lambda (t-d)
                   (subset? (second t-d) sample))
                 theorem-deps)))

  (map normed-theorem-of theorem-files-admitted))

;; Return equational theorems (filenames, one per file) which would be possible
;; to discover given what's in the provided sample. In other words, those
;; theorems which are equations, and whose dependencies are a subset of the
;; sample.
(define (equations-admitted-by-sample sample)
  (filter (lambda (thm)
            (not (empty? (theorem-to-equation thm))))
          (conjectures-admitted-by sample)))

;; Lexicographic comparison of two structures. We only focus on nested lists of
;; symbols.
(define (lex<=? x y)
  (cond
   ;; Compare symbols lexicographically
   [(and (symbol? x) (symbol? y)) (symbol<=? x y)]

   ;; Symbols are smaller than other structures
   [(symbol? x) #t]
   [(symbol? y) #f]

   ;; Numbers are the second-smallest type (only Reals, due to <='s contract).
   [(and (real? x) (real? y)) (<= x y)]
   [(real? x) #t]
   [(real? y) #f]

   ;; Strings are next
   [(and (string? x) (string? y)) (string<=? x y)]
   [(string? x) #t]
   [(string? y) #f]

   ;; If they're not symbols or strings, they must be lists
   [(not (list? x)) (error (format "Expected list, got ~s" x))]
   [(not (list? y)) (error (format "Expected list, got ~s" y))]

   ;; Empty lists are the smallest lists
   [(empty? x) #t]
   [(empty? y) #f]

   ;; Recurse on first elements; if not <=, they can't be equal wither, so stop.
   [(not (lex<=? (first x) (first y))) #f]

   ;; (first x) <= (first y), but are they equal? If not symmetric, we can stop.
   [(not (lex<=? (first y) (first x))) #t]

   ;; First elements are equal, recurse to the rest of the lists
   [else (lex<=? (rest x) (rest y))]))

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
    [`(,sym (= ,lhs ,rhs)) (if (string-suffix? (symbol->string sym)
                                               "custom-bool-converter")
                               (let ([x (to-expression lhs)]
                                     [y (to-expression rhs)])
                                 (if (or (empty? x) (empty? y))
                                     '()
                                     (if (lex<=? (first x) (first y))
                                         (list (list '~= (first x) (first y)))
                                         (list (list '~= (first y) (first x))))))
                               (error (format "Equation without wrapper ~s"
                                              `(,sym (= ,lhs ,rhs)))))]

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
               (not (member (first x) native-symbols))))) '()]))

;; Try to parse the given string as a JSON representation of an equation, e.g.
;; from reduce-equations. Returns a list containing the result on success, or an
;; empty list on failure.
(define/test-contract (parse-json-equation str)
  (-> string? (or/c (list/c equation?)
                    empty?))
  (with-handlers ([exn:fail:read? (lambda (e) '())])
    (parse-equation (string->jsexpr str))))

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

;; FIXME: Add tests, add checks for whether input conforms to expected format, etc.
(define (precision-wrapper)
  (define sample
    (read-benchmark (getenv "SAMPLED_NAMES")))

  (write (precision-from-sample (parse-json-equations (port->string))
                                sample)))

;; FIXME: Add tests, add checks for whether input conforms to expected format, etc.
(define (recall-wrapper)
  (define sample
    (read-benchmark (getenv "SAMPLED_NAMES")))

  (write (recall-from-sample (parse-json-equations (port->string))
                             sample)))

;; Everything below here is tests; run using "raco test"
(module+ test
  (require rackunit)

  ;; Suppress normalisation progress messages during tests
  (quiet)

  ;; For testing, we default to only using a subset of the benchmarks, which we
  ;; accomplish by overriding theorem-files; this acts as a sanity check, and is
  ;; much faster than checking everything. For a thorough test of all benchmarks
  ;; there is a separate "tests" derivation in default.nix, suitable for use in
  ;; e.g. a continuous integration scenario.

  ;; Please note the following:
  ;;  - If a particular set of benchmarks has specifically been requested, via
  ;;    the BENCHMARKS environment variable, we use that whole set, rather than
  ;;    selecting some subset.
  ;;  - If a test requires some particular file to be present in this list, it
  ;;    should use the testing-file function to check that it's present.
  (define testing-file
    (let ()
      ;; We always include the following files, since they're either required by
      ;; one of our tests, or they're edge-cases/regressions which we want to
      ;; ensure are getting regularly tested.
      (define required-testing-files
        (benchmark-files '("grammars/packrat_unambigPackrat.smt2"
                           "isaplanner/prop_01.smt2"
                           "isaplanner/prop_15.smt2"
                           "isaplanner/prop_35.smt2"
                           "isaplanner/prop_43.smt2"
                           "isaplanner/prop_44.smt2"
                           "isaplanner/prop_84.smt2"
                           "prod/prop_35.smt2"
                           "tip2015/fermat_last.smt2"
                           "tip2015/heap_SortPermutes'.smt2"
                           "tip2015/list_SelectPermutations.smt2"
                           "tip2015/nat_pow_one.smt2"
                           "tip2015/propositional_AndCommutative.smt2"
                           "tip2015/propositional_AndIdempotent.smt2"
                           "tip2015/sort_NStoogeSort2Count.smt2"
                           "tip2015/sort_NStoogeSort2Permutes.smt2"
                           "tip2015/tree_sort_SortPermutes'.smt2")))

      ;; Override theorem-files to return these chosen files, if no BENCHMARKS
      ;; were given explicitly
      (when (member (getenv "BENCHMARKS") '(#f ""))
        (set-theorem-files! (lambda ()
                              required-testing-files)))

      ;; The definition of testing-file; checks if the given file is in our
      ;; selected list.
      (lambda (f)
        (unless (member (benchmark-file f) required-testing-files)
          (error "Testing file not in required list" f))
        f)))

  ;; Loads test data from files
  (define (test-data f)
    (when (member (getenv "TEST_DATA") '(#f ""))
      (error "No TEST_DATA env var given"))
    (string-append (getenv "TEST_DATA") "/" f))

  ;; Select specific test-cases based on regex
  (define test-case-regex
    ;; Match everything if no regex given
    (let ([given (or (getenv "PLT_TEST_REGEX") "")])
      (match given
        ["" #rx".*"]
        [_  (regexp given)])))

  (define-syntax-rule (def-test-case name body ...)
    (when (regexp-match? test-case-regex name)
      (test-case name body ...)))

  ;; Examples used for tests
  (define nat-def      '(declare-datatypes () ((Nat (Z) (S (p Nat))))))

  (define constructorZ '(define-fun constructor-Z ()              Nat (as Z Nat)))

  (define constructorS '(define-fun constructor-S ((local-p Nat)) Nat (as (S local-p) Nat)))

  (define redundancies `(,constructorZ
                         ,constructorS
                         (define-fun redundantZ1 () Nat (as Z Nat))
                         (define-fun redundantZ2 () Nat (as Z Nat))
                         (define-fun redundantZ3 () Nat (as Z Nat))))

  (define custom-bool
    '(declare-datatypes () ((CustomBool (CustomTrue) (CustomFalse)))))

  (define custom-ite
    '(define-fun
       (par (a)
            (custom-ite
             ((c CustomBool) (x a) (y a)) a
             (match c
               (case CustomTrue  x)
               (case CustomFalse y))))))

  (define custom-nat
    '(declare-datatypes () ((CustomNat (CustomZ)
                                       (CustomS (custom-p CustomNat))))))

  (define custom-int
    '(declare-datatypes () ((CustomInt (CustomNeg (custom-succ CustomNat))
                                       (CustomZero)
                                       (CustomPos (custom-pred CustomNat))))))

  (define test-benchmark-defs
    (mk-defs-s (theorem-files)))

  (def-test-case "List manipulation"
    (check-equal? (symbols-in '(lambda ((local1 Nat) (local2 (List Nat)))
                                 (free1 local1)))
                  '(free1))

    (check-equal? (concat-map (lambda (x) (list x x x))
                              '(fee fi fo fum))
                  '(fee fee fee fi fi fi fo fo fo fum fum fum)))

  (check-equal? (add-constructor-funcs (list nat-def))
                `(,nat-def ,constructorZ ,constructorS))

  (for-each (lambda (f)
              (check-equal? (map ~a (mk-defs-s (string-split f "\n")))
                            (string-split (string-trim (pipe f mk-defs)) "\n")))
            (benchmark-files '("grammars/simp_expr_unambig1.smt2"
                               "grammars/simp_expr_unambig4.smt2"
                               "tip2015/sort_StoogeSort2IsSort.smt2")))

  (check-equal? (trim '((hello)))                      '((hello)))
  (check-equal? (trim '((foo) (assert-not bar) (baz))) '((foo) (baz)))
  (check-equal? (trim '((foo) (check-sat) (bar)))      '((foo) (bar)))

  (define f
    (benchmark-file "grammars/simp_expr_unambig3.smt2"))

  (define one-liners
    (qual-all (string-split f "\n")))

  (define result
    (filter non-empty?
            (map (lambda (expr)
                   (filter non-empty? (names-in (list expr))))
                 one-liners)))

  (let ([all-result (filter non-empty? (map names-in one-liners))])
    (with-check-info
     (('f          f)
      ('one-liners one-liners)
      ('result     result)
      ('all-result all-result))
     (check-equal? all-result result)))

  (check-true  (ss-eq? 'foo  'foo))
  (check-true  (ss-eq? 'foo  "foo"))
  (check-true  (ss-eq? "foo" 'foo))
  (check-true  (ss-eq? "foo" "foo"))

  (check-false (ss-eq? 'foo  'bar))
  (check-false (ss-eq? 'foo  "bar"))
  (check-false (ss-eq? "foo" 'bar))
  (check-false (ss-eq? "foo" "bar"))

  (check-equal? (find-sub-exprs 'constructor-Z
                                `(,nat-def ,constructorZ ,constructorS))
                (list constructorZ))

  (let* ([file "tip2015/sort_StoogeSort2IsSort.smt2"]
         [defs (mk-defs-s (list (benchmark-file file)))])
    (for-each (lambda (data)
                (define def
                  (find-sub-exprs (string-append file (first data) "-sentinel")
                                  defs))

                (with-check-info
                 (('defs    defs)
                  ('def     def )
                  ('name    (first  data))
                  ('kind    (second data))
                  ('message "Can get function definition"))
                 (check-equal? (length def) 1)))
              '(("sort2"        "plain")
                ("insert2"      "recursive")
                ("zsplitAt"     "parameterised")
                ("ztake"        "parameterised recursive")
                ("stooge2sort2" "mutually recursive"))))

  (define test-files
    (benchmark-files '("grammars/simp_expr_unambig1.smt2"
                       "grammars/simp_expr_unambig4.smt2"
                       "tip2015/sort_StoogeSort2IsSort.smt2"
                       "tip2015/sort_BSortPermutes.smt2")))

  (define test-defs
    (mk-defs-s test-files))

  (def-test-case "Real symbols qualified"
    (let* ([fs (benchmark-files '("tip2015/propositional_AndCommutative.smt2"
                                  "tip2015/propositional_Sound.smt2"
                                  "tip2015/propositional_Okay.smt2"
                                  "tip2015/regexp_RecSeq.smt2"
                                  "tip2015/relaxedprefix_correct.smt2"
                                  "tip2015/propositional_AndIdempotent.smt2"
                                  "tip2015/propositional_AndImplication.smt2"))]
           [q (qual-all fs)]
           [s (symbols-of-theorems-s q)])

      (check-true (string-contains? (~a s) "or2-sentinel")
                  "Found an or2 symbol")

      (check-false (member 'or2-sentinel s)
                   "or2 symbol is qualified")

      (check-true (string-contains? (format-symbols
                                     (symbols-of-theorems-s (mk-defs-s fs)))
                                    "or2-sentinel")
                  "Found 'or2' symbol")))

  (define subset '(grammars/simp_expr_unambig1.smt2append-sentinel
                   grammars/simp_expr_unambig1.smt2lin-sentinel
                   grammars/simp_expr_unambig4.smt2nil-sentinel
                   grammars/simp_expr_unambig4.smt2cons-sentinel
                   grammars/simp_expr_unambig4.smt2C-sentinel
                   grammars/simp_expr_unambig4.smt2D-sentinel
                   grammars/simp_expr_unambig4.smt2X-sentinel
                   grammars/simp_expr_unambig4.smt2Y-sentinel
                   grammars/simp_expr_unambig4.smt2Pl-sentinel
                   grammars/simp_expr_unambig4.smt2Plus-sentinel
                   grammars/simp_expr_unambig4.smt2EX-sentinel
                   grammars/simp_expr_unambig4.smt2EY-sentinel
                   grammars/simp_expr_unambig4.smt2head-sentinel
                   grammars/simp_expr_unambig4.smt2tail-sentinel
                   grammars/simp_expr_unambig4.smt2Plus_0-sentinel
                   grammars/simp_expr_unambig4.smt2Plus_1-sentinel
                   grammars/simp_expr_unambig4.smt2append-sentinel
                   grammars/simp_expr_unambig4.smt2linTerm-sentinel
                   grammars/simp_expr_unambig4.smt2lin-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2nil-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2cons-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2sort2-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2insert2-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2zsplitAt-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2ztake-sentinel
                   tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2-sentinel))

  (define qual (format-symbols (qual-all test-files)))

  (let ([syms (symbols-of-theorems-s (read-benchmark qual))])

    (for-each (lambda (sym)
                (with-check-info
                 (('sym     sym)
                  ('message "Native symbol was stripped"))
                 (check-false (member sym syms))))
              '(true-sentinel false-sentinel ite-sentinel or-sentinel))

    (for-each (lambda (sym)
                (with-check-info
                 (('sym     sym)
                  ('syms    syms)
                  ('message "Found symbol"))
                 (check-not-equal? (member sym syms)
                                   #f)))
              subset))

  (let ([syms (symbols-of-theorems-s test-defs)])
    (for-each (lambda (sym)
                (with-check-info
                  (('sym       sym)
                   ('test-defs test-defs)
                   ('message   "Symbol is qualified"))
                  (check-true (string-contains? (symbol->string sym)
                                                ".smt2")))

                (with-check-info
                  (('sym       sym)
                   ('test-defs test-defs)
                   ('message   "Symbol has suffix"))
                  (check-true (string-contains? (symbol->string sym)
                                                "-sentinel"))))
              syms))

  (def-test-case "No alpha-equivalent duplicates in result"
    (let ([normalised (norm test-defs)])
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

  (check-equal? (get-def-s 'constructor-Z redundancies)
                (list constructorZ))

  (def-test-case "Can find constructors"
    (for-each (lambda (c)
                (define found
                  (get-def-s c test-benchmark-defs))
                (with-check-info
                  (('found found))
                  (check-equal? (length found) 1)))
              (expression-constructors test-benchmark-defs)))

  (for-each (lambda (sym)
    (define def (get-def sym qual))

    (let ([count (length def)])
      (with-check-info
       (('sym     sym)
        ('def     def)
        ('count   count)
        ('message "Symbol got qualified"))
       (check-equal? count 1)))

    (define norm-def
      (get-def-s sym test-defs))

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

    ;; Collect together some definitions, which include some alpha-equivalent
    ;; redundancies
    (define files (append test-files
                          (theorem-files)
                          (benchmark-files
                           '("tip2015/sort_StoogeSort2IsSort.smt2"
                             "isaplanner/prop_58.smt2"
                             "tip2015/list_PairUnpair.smt2"))))

    (define raw       (qual-all files))
    (define raw-names (names-in raw))

    ;; Remove redundancies
    (define normal       (mk-final-defs-s files))
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
                  (map remove-suffix (names-in raw-def)))

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
                                             ('normal-name     (decode-name normal-name-enc))
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

    (check-equal? (list->set
                   (find-redundancies
                    '((declare-datatypes
                       (local-a local-b)
                       ((isaplanner/prop_58.smt2Pair-sentinel
                         (isaplanner/prop_58.smt2Pair2-sentinel
                          (isaplanner/prop_58.smt2first-sentinel local-a)
                          (isaplanner/prop_58.smt2second-sentinel local-b)))))
                      (declare-datatypes
                       (local-a local-b)
                       ((tip2015/sort_StoogeSort2IsSort.smt2Pair
                         (tip2015/sort_StoogeSort2IsSort.smt2Pair2
                          (tip2015/sort_StoogeSort2IsSort.smt2first local-a)
                          (tip2015/sort_StoogeSort2IsSort.smt2second local-b))))))))
                  (list->set
                   '((tip2015/sort_StoogeSort2IsSort.smt2Pair
                      isaplanner/prop_58.smt2Pair-sentinel)
                     (tip2015/sort_StoogeSort2IsSort.smt2Pair2
                      isaplanner/prop_58.smt2Pair2-sentinel)
                     (tip2015/sort_StoogeSort2IsSort.smt2first
                      isaplanner/prop_58.smt2first-sentinel)
                     (tip2015/sort_StoogeSort2IsSort.smt2second
                      isaplanner/prop_58.smt2second-sentinel)))))

  (check-equal? (names-in '(fee fi fo fum))
                '())
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

  (define qualified-example
    '((define-fun (par (a b)
                       (foo.smt2baz-sentinel ((x Nat)) Nat
                                             X)))

      (define-fun
        foo.smt2quux-sentinel () Bool (hello world))

      (define-fun-rec (par (a)
                           (bar.smt2quux-sentinel () Foo
                                                  (foo bar))))))

  (check-equal? (unqualify qualified-example)
                '((define-fun (par (a b)
                                   (foo.smt2baz  ((x Nat)) Nat
                                                 X)))
                  (define-fun
                    foo.smt2quux ()        Bool
                    (hello world))
                  (define-fun-rec (par (a)
                                       (bar.smt2quux ()        Foo
                                                     (foo bar))))))

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
                  (check-sat)))

  (def-test-case "Find redundancies"
    ;; Check known expression
    (check-equal? (list->set (find-redundancies redundancies))
                  (list->set '((redundantZ1 constructor-Z)
                               (redundantZ2 constructor-Z)
                               (redundantZ3 constructor-Z))))

    ;; Ensure we keep the lexicographically-smallest name
    (check-equal? (list->set (find-redundancies
                              '((declare-datatypes () ((TB (C1A) (C2C (D1B TB)))))
                                (declare-datatypes () ((TC (C1C) (C2B (D1A TC)))))
                                (declare-datatypes () ((TA (C1B) (C2A (D1C TA))))))))
                  (list->set '((TB  TA)
                               (TC  TA)
                               (C1B C1A)
                               (C1C C1A)
                               (C2B C2A)
                               (C2C C2A)
                               (D1B D1A)
                               (D1C D1A)))))

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

  (check-equal? (replace-strings "hello mellow yellow fellow"
                                 '(("lo" "LO") ("el" "{{el}}")))
                "h{{el}}LO m{{el}}LOw y{{el}}LOw f{{el}}LOw")

  (check-equal? (list->set (read-benchmark
                            (replace-strings (format-symbols      redundancies)
                                             (find-redundancies redundancies))))
                (list->set `(,constructorZ ,constructorS)))

  (let* ([given '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                  (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b)))]
         [defs  (norm-defs given)]
         [syms  (symbols-of-theorems-s defs)]
         [min1  (member 'min1 syms)]
         [min2  (member 'min2 syms)])
    (with-check-info
     (('defs    defs)
      ('syms    syms)
      ('min1    min1)
      ('min2    min2)
      ('message "Simple redundant functions deduped"))
     (check-true (or (and min1 (not min2))
                     (and min2 (not min1))))))

  (let* ([given '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                  (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))
                  (define-fun fun3 ((x Int)) Int (min2 x x)))]
         [defs  (norm-defs given)]
         [syms  (symbols-of-theorems-s
                 (filter (lambda (expr)
                           (member 'fun3 expr))
                         defs))])
    (with-check-info
     (('defs    defs)
      ('syms    syms)
      ('message "References to discarded duplicates are replaced"))
     (check-not-equal? (member 'min1 syms) #f)))

  (define (in-temp-dir f)
    (let* ([dir    (make-temporary-file (string-append temp-file-prefix "~a")
                                        'directory)]
           [result (f dir)])
      (delete-directory/files dir)
      result))

  (define (string-to-haskell vals)
    ;; mk-final-defs takes in filenames, so it can qualify names. This makes
    ;; and cleans up temporary files for testing.

    ;; Note: We make a file in a directory, to avoid problems if tmpdir begins
    ;; with a number (e.g. '/var/run/user/1000'); otherwise qualified variable
    ;; names would be invalid
    (in-temp-dir (lambda (dir)
                   (define temp-file (string-append (path->string dir)
                                                    "/test.smt2"))

                   (for-each (lambda (val)
                               (display-to-file val  temp-file
                                                #:exists 'append)
                               (display-to-file "\n" temp-file
                                                #:exists 'append))
                             vals)

                   (let* ([result (defs-to-sig temp-file)])
                     (delete-file temp-file)
                     result))))

  (define form-deps
    (list custom-nat custom-int))

  (define form
    '(declare-datatypes ()
                        ((Form (& (&_0 Form) (&_1 Form))
                               (Not (Not_0 Form))
                               (Var (Var_0 CustomInt))))))

  (define form-with-deps
    (append form-deps (list form)))

  (def-test-case "Can get form constructors"
    (check-equal? (get-def-s '& form-with-deps)
                  (list form)))

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

  (def-test-case "Form datatype survives translation"
    (define sig (string-to-haskell form-with-deps))

    (define data-found
      (regexp-match? "data[ ]+Global[0-9a-f]+[ ]+="
                     (string-replace sig "\n" "")))

    (with-check-info
      (('data-found data-found)
       ('sig        sig)
       ('message    "'Form' datatype appears in signature"))
      (check-true data-found)))

  (def-test-case "Mutual recursion"
    (define mut (list custom-bool
                      custom-nat
                      custom-int
                      custom-ite
                      '(define-funs-rec
                         ((models  ((x CustomBool)
                                    (y CustomInt))
                                   CustomBool)
                          (models2 ((q CustomBool)
                                    (x CustomInt))
                                   CustomBool)
                          (models5 ((q CustomBool)
                                    (x CustomInt)
                                    (y CustomInt))
                                   CustomBool))

                         ((custom-ite x
                                      (models2 (models x y) y)
                                      (models5 (models x y) y y))

                          (custom-ite q
                                      (models5 (models q x) x x)
                                      (models2 (models q x) x))

                          (custom-ite q
                                      (models2 q x)
                                      (models5 q x y))))))

    (define prepared (prepare mut))

    (with-check-info
     (('prepared prepared)
      ('message "Mutually-recursive function definitions survive preparation"))
    (check-not-equal? #f
                      (member 'define-funs-rec (flatten prepared)))

    (define sig (string-to-haskell mut))

    (for-each
     (lambda (fun)
       (define def-found
         (any-of (lambda (line)
                   (and (string-prefix? line "global")
                        (let* ([id   (first (regexp-match "global[0-9a-f]+"
                                                          line))]
                               [enc  (substring id 6)]
                               [name (decode16 enc)])
                          (regexp-match? (string-append
                                          "^"
                                          (regexp-quote temp-file-prefix)
                                          "[0-9]+"
                                          (regexp-quote (string-append "/test.smt2" fun))
                                          "$")
                                         name))))
                 (string-split sig "\n")))

       (with-check-info
         (('fun       fun)
          ('enc       (encode16 fun))
          ('def-found def-found)
          ('sig       sig)
          ('message   "Function defined in signature"))
         (check-true def-found)))
     (list "models" "models2" "models5"))))

  (def-test-case "Single files"
    (define files (map (lambda (suf)
                         (benchmark-file suf))
                       '(;"isaplanner/prop_54.smt2"
                         ;"tip2015/propositional_AndIdempotent.smt2"
                         ;"tip2015/propositional_AndCommutative.smt2"
                         ;"tip2015/mccarthy91_M2.smt2"
                         ;"isaplanner/prop_36.smt2"
                         ;"tip2015/sort_MSortTDPermutes.smt2"
                         "tip2015/tree_sort_SortPermutes'.smt2"
                         ;"tip2015/sort_StoogeSort2Permutes.smt2"
                         ;"tip2015/sort_StoogeSortPermutes.smt2"
                         ;"tip2015/polyrec_seq_index.smt2"
                         #;"tip2015/sort_QSortPermutes.smt2")))

    (for-each (lambda (f)
                (define sig
                  (defs-to-sig f))
                (with-check-info
                  (('sig sig))
                  (check-true (string-contains? sig "QuickSpec"))))
              files))

  (def-test-case "Multiple files"
    (define files
      (string-join (benchmark-files '("tip2015/tree_SwapAB.smt2"
                                      "tip2015/list_SelectPermutations.smt2"))
                   "\n"))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('message "Local variables renamed")
      ('sig     sig))
     (check-true (string-contains? sig "local"))))

  (def-test-case "Random files"
    (define files
      (string-join (theorem-files) "\n"))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('files   files)
      ('sig     sig)
      ('message "Made Haskell for random files"))
     (check-true (string-contains? sig "QuickSpec"))))

  (let ([a (getenv "HOME")]
        [b (parameterize-env '([#"HOME" #"foo"])
                             (lambda () (getenv "HOME")))]
        [c (getenv "HOME")])
    (check-equal? a c)
    (check-equal? b "foo"))

  (def-test-case "Module tests"
    (define files
      (string-join (theorem-files) "\n"))

    (in-temp-dir
     (lambda (dir)
       (define out-dir (path->string dir))
       (parameterize-env `([#"FILES"   ,(string->bytes/utf-8 files)]
                           [#"OUT_DIR" ,(string->bytes/utf-8 out-dir)])
         (lambda ()
           (full-haskell-package-s
            (format-symbols (mk-final-defs-s (theorem-files)))
            out-dir)

           (with-check-info
            (('files   files)
             ('message "Made Haskell package"))
            (check-true (directory-exists? out-dir)))))

       (with-check-info
        (('message "Made src directory"))
        (check-true (directory-exists? (string-append out-dir "/src"))))

       (for-each (lambda (f)
                   (with-check-info
                    (('f       f)
                     ('message "Made package file"))
                    (check-true (file-exists? (string-append out-dir f)))))
                 '("/src/A.hs" "/tip-benchmark-sig.cabal" "/LICENSE"))

       (parameterize-env `([#"HOME" ,(string->bytes/utf-8 out-dir)])
         (lambda ()
           (define stdout (open-output-string))
           (define stderr (open-output-string))
           (define output "nothing")
           (parameterize ([current-directory out-dir]
                          [current-output-port stdout]
                          [current-error-port  stderr])
             (run-pipeline '(cabal configure))

             (set! output
               (run-pipeline/out '(echo -e "import A\n:browse")
                                 '(cabal repl -v0))))

           ;; If the import fails, we're stuck with the Prelude, which
           ;; contains classes like Functor
           (with-check-info
             (('output  output)
              ('stdout  (get-output-string stdout))
              ('stderr  (get-output-string stderr))
              ('message "Module imported successfully"))
             (check-false (string-contains? output "class Functor"))))))))

  (define regressions
    (benchmark-files '("tip2015/propositional_AndCommutative.smt2")))

  (for-each (lambda (file)
    (define name  (last (string-split file "/")))
    (define name2 (string-replace name "'" "_tick_"))
    (define syms  (symbols-of-theorems-s
                   (qualify name2
                            (file->list file))))

    (with-check-info
     (('file    file)
      ('name2   name2)
      ('syms    syms)
      ('message "Symbols qualified"))
     (check-equal? '()
                   (filter (lambda (sym)
                             (and (not (empty? sym))
                                  (not (string-contains? (~a sym) name2))))
                           syms))))
    (append regressions (theorem-files)))

  (def-test-case "Haskell package made"
    (in-temp-dir
     (lambda (out-dir)
       (parameterize-env `([#"FILES"   ,(string->bytes/utf-8
                                         (string-join (theorem-files) "\n"))]
                           [#"OUT_DIR" ,(string->bytes/utf-8
                                         (path->string out-dir))]
                           [#"HOME"    ,(string->bytes/utf-8
                                         (path->string out-dir))])
         (lambda ()
           (full-haskell-package-s (format-symbols (mk-final-defs-s (theorem-files)))
                                   (path->string out-dir))

           (parameterize ([current-directory out-dir])

             (check-true (directory-exists? "src"))

             (check-true (file-exists? "src/A.hs"))

             (check-true (file-exists? "tip-benchmark-sig.cabal"))

             (check-true (file-exists? "LICENSE"))

             (run-pipeline/out '(cabal configure))

             (define out
               (run-pipeline/out '(echo -e "import A\n:browse")
                                 '(cabal repl -v0)))

             ;; If the import fails, we're stuck with
             ;; the Prelude, which contains classes
             (check-false (string-contains? out
                                            "class Functor"))))))))

  (define (names-match src expr expect)
    (define names (names-in expr))

    (with-check-info
     (('src     src)
      ('expr    expr)
      ('expect  expect)
      ('names   names)
      ('message "Got expected names"))
    (check-true (set-equal? names expect))))

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
               '(stooge2sort2 stoogesort2 stooge2sort1))

  (def-test-case "Symbol lookup"
    (define (symbols-from-file f)
      (names-in (file->list f)))

    (define (contains lst elem)
      (not (empty? (filter (curry equal? elem)
                           lst))))

    (define (should-have syms kind xs)
      (for-each (lambda (sym)
                  (with-check-info
                   (('sym  sym)
                    ('syms syms)
                    ('kind kind))
                   (check-true (contains syms sym))))
                xs))

    (define (should-not-have syms kind xs)
      (for-each (lambda (sym)
                  (with-check-info
                   (('sym  sym)
                    ('syms syms)
                    ('kind kind))
                   (check-false (contains syms sym))))
                xs))

    (let* ([f    (benchmark-file "tip2015/int_right_distrib.smt2")]
           [syms (symbols-from-file f)])

      (should-have syms 'constructor '(Pos Neg Zero Succ P N))

      (should-have syms 'destructor  '(pred P_0 N_0))

      (should-have syms 'function '(toInteger sign plus2 opposite
                                    timesSign mult minus plus absVal
                                    times))

      (should-not-have syms 'variable '(x  x-sentinel
                                        y  y-sentinel
                                        z  z-sentinel
                                        m  m-sentinel
                                        m2 m2-sentinel
                                        n  n-sentinel
                                        n2 n2-sentinel
                                        n3 n3-sentinel
                                        o  o-sentinel))

      (should-not-have syms 'keyword  '(match             match-sentinel
                                        case              case-sentinel
                                        define-fun        define-fun-sentinel
                                        declare-datatypes declare-datatypes-sentinel
                                        assert-not        assert-not-sentinel
                                        forall            forall-sentinel
                                        =                 =-sentinel
                                        check-sat         check-sat-sentinel)))

    (let* ([f    (benchmark-file "tip2015/list_PairEvens.smt2")]
           [syms (symbols-from-file f)])
      (should-not-have syms 'higher-order-type '(=> =>-sentinel)))

    (let* ([f    (benchmark-file "tip2015/propositional_AndCommutative.smt2")]
           [syms (symbols-from-file f)])
      (should-have syms 'function '(or2))))

  (let ([f (benchmark-file "tip2015/nat_alt_mul_comm.smt2")])
    (check-equal? (string-trim (pipe (file->string f) types-from-defs))
                  "CustomBool\nNat"))

  (def-test-case "Name extraction"
    (check-equal?
     (lowercase-names
      (file->list (benchmark-file "tip2015/sort_NStoogeSort2Permutes.smt2")))
     '(custom-p custom-succ custom-pred custom-ite custom-not custom-nat->
       custom-> custom-<= custom-or custom-bool-converter custom-and head tail
       first second p twoThirds third take sort2 null length elem drop splitAt
       delete isPermutation append nstooge2sort2 nstoogesort2 nstooge2sort1))

    (check-equal?
     (uppercase-names
      (file->list (benchmark-file "tip2015/sort_NStoogeSort2Permutes.smt2")))
     '(CustomNat CustomZ CustomS CustomInt CustomNeg CustomZero CustomPos
       CustomBool CustomTrue CustomFalse list nil cons Pair Pair2 Nat Z S)))

  ;; When TIP translates from its smtlib-like format to Haskell, it performs a
  ;; renaming step, to ensure that all names are valid Haskell identifiers. We
  ;; need to ensure that the names we produce don't get altered by this step.
  (def-test-case "Name preservation"
    (define test-benchmark-defs
      (mk-final-defs-s (theorem-files)))

    (define test-benchmark-lower-names
      ;; A selection of names, which will be lowercase in Haskell
      (concat-map lowercase-names test-benchmark-defs))

    (define test-benchmark-upper-names
      ;; A selection of names, which will be uppercase in Haskell
      (concat-map uppercase-names test-benchmark-defs))

    (define (tip-lower-rename name)
      "Given a function name NAME, returns TIP's renamed version"

      (define input
        ;; A trivial definition which uses this name
        `((define-fun-rec ,name () Bool ,name)
          (check-sat)))

      (define output
        ;; Run through TIP
        (run-pipeline/out `(echo ,(format-symbols input))
                          '(tip --haskell)))

      ;; The definition will be on the only line with an "="
      (define def-line
        (first (filter (lambda (line)
                         (string-contains? line "="))
                       (string-split output "\n"))))

      ;; The name will be the only thing to the left of the "="
      (string-trim (first (string-split def-line "="))))

    (define (tip-upper-rename name)
      "Given a type name NAME, returns TIP's renamed version"

      (define input
        ;; A trivial definition which uses this name
        `((declare-datatypes () ((,name)))
          (check-sat)))

      (define output
        ;; Run through TIP
        (run-pipeline/out `(echo ,(format-symbols input))
                          '(tip --haskell)))

      ;; The definition will be immediately to the left of the only occurrence
      ;; of "="
      (define prefix
        (string-trim (first (string-split (string-replace output "\n" "")
                                          "="))))

      ;; The name will occur immediately after the final "data"
      (string-trim (last (string-split prefix "data"))))

    (for-each (lambda (name)
                (check-equal? (tip-lower-rename name) (symbol->string name)))
              test-benchmark-lower-names)

    (for-each (lambda (name)
                (check-equal? (tip-upper-rename name) (symbol->string name)))
              test-benchmark-upper-names))

  (def-test-case "Can't reference unbound names"
    (check-exn #rx"tip: Parse failed: Symbol bar .* not bound"
               (lambda ()
                 (mk-signature-s
                  (format-symbols
                   '((declare-datatypes
                      (a)
                      ((List (Nil) (Cons (head a) (tail (List a))))))
                     (define-fun
                       (par (a) (foo ((x (List a))) a
                                     (as (bar x) a))))
                     (check-sat)))))))

  (def-test-case "Can reference destructor names"
    (check-true (string? (mk-signature-s
                          (format-symbols
                           '((declare-datatypes
                              (a)
                              ((List (Nil) (Cons (head a) (tail (List a))))))
                             (define-fun
                               (par (a) (foo ((x (List a))) a
                                             (as (head x) a))))
                             (check-sat)))))))

  (def-test-case "Can't reuse destructor names"
    (check-exn #rx"tip: Parse failed: Symbol head .* is already globally bound"
               (lambda ()
                 (mk-signature-s
                  (format-symbols
                   '((declare-datatypes
                      (a)
                      ((List (Nil) (Cons (head a) (tail (List a))))))
                     (define-fun
                       (par (a) (head ((x (List a))) a
                                      (match x
                                        (case (Cons y zs) y)))))
                     (check-sat)))))))

  (def-test-case "Decode strings"
    (check-equal? (decode-string "foo Global68656c6c6f bar global776f726c64")
                  "foo hello bar world"))

  (def-test-case "Bare constructor function type"
    (check-equal? (add-constructor-funcs '((declare-datatypes
                                            ()
                                            ((MyBool (MyTrue) (MyFalse))))))
                  '((declare-datatypes
                     ()
                     ((MyBool (MyTrue) (MyFalse))))
                    (define-fun constructor-MyTrue  () MyBool (as MyTrue  MyBool))
                    (define-fun constructor-MyFalse () MyBool (as MyFalse MyBool)))))

  (def-test-case "Parameterised constructor function type"
    (check-equal? (add-constructor-funcs '((declare-datatypes
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
                          ((local-myHead local-a) (local-myTail (MyStream local-a)))
                          (MyStream local-a)
                          (as (MyCons local-myHead local-myTail) (MyStream local-a))))))))

  (def-test-case "Bare destructor function type"
    (check-equal? (add-destructor-funcs '((declare-datatypes
                                           ()
                                           ((Nat (Z) (S (p Nat)))))))
                  '((declare-datatypes
                     ()
                     ((Nat (Z) (S (p Nat)))))
                    (define-fun destructor-p ((destructor-arg Nat)) Nat
                      (match destructor-arg
                        (case (S local-p) local-p))))))

  (def-test-case "Parameterised destructor function type"
    (check-equal? (add-destructor-funcs '((declare-datatypes
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
                              (case (Many local-head local-tail) local-tail))))))))

  (def-test-case "Can extract theorems"
    (for-each (lambda (benchmark-file)
                (define thms (theorems-from-file benchmark-file))

                (define content (file->list benchmark-file))

                (with-check-info
                  (('benchmark-file benchmark-file)
                   ('thms           thms))
                  (check-equal? (length thms) 1))

                (with-check-info
                  (('benchmark-file benchmark-file)
                   ('thms           thms)
                   ('content        content))
                  (check-not-equal? (member (car thms) content) #f)))
              (append (theorem-files) (theorem-files))))

  (def-test-case "Have benchmark theorems"
    (for-each (lambda (benchmark-file)
                (check-not-equal? (theorem-of benchmark-file)
                                  #f))
              (theorem-files)))

  (def-test-case "Have replacements"
    (define defs '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                   (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b))))
    (check-equal? (list->set (replacements-closure defs))
                  (list->set '((min2 min1))))

    (define test-replacements
      (replacements-closure (qual-all (theorem-files))))

    (for-each (lambda (rep)
                (check-false         (member (first rep)
                                             (names-in test-benchmark-defs)))
                (check-not-equal? #f (member (second rep)
                                             (names-in test-benchmark-defs))))
              test-replacements))

  (def-test-case "Normalise theorems"
    (define (structure-of expr)
      (match expr
        [(cons x y) (cons (structure-of x) (structure-of y))]
        [_          #f]))

    (for-each (lambda (benchmark-file)
                (define unnormed
                  (theorem-of benchmark-file))
                (define normed
                  (normed-theorem-of benchmark-file))

                (check-equal? (structure-of unnormed) (structure-of normed))

                (check-false (string-contains? (format-symbols normed)
                                               "-sentinel")))
              (theorem-files)))

  (def-test-case "Theorem deps"
    (for-each (lambda (name-cases)
                (for-each (lambda (f-deps)
                            (define absolute-path
                              (benchmark-file (first f-deps)))

                            (define (prefix s)
                              (string->symbol
                               (string-append (first f-deps)
                                              (symbol->string s))))

                            (define calc-deps
                              (theorem-deps-of absolute-path))

                            (with-check-info
                              (('sort          (first name-cases))
                               ('absolute-path absolute-path)
                               ('deps          (second f-deps))
                               ('calc-deps     calc-deps))
                              (check-equal? (list->set calc-deps)
                                            (list->set (second f-deps)))))
                          (second name-cases)))

              ;; Cases are grouped by type, e.g. whether they require
              ;; constructor function replacement.
              ;; Each case has a filename and a list of expected dependencies;
              ;; we use testing-file to ensure these files are included in the
              ;; subset of files we're testing with. Since some names might
              ;; normalise differently in the presence of files with
              ;; lexicographically-smaller paths, we also ensure the canonical
              ;; definitions are included (inside the begin blocks).
              `(("Simple"
                 ((,(begin
                      (testing-file "tip2015/list_SelectPermutations.smt2")
                      (testing-file "tip2015/sort_NStoogeSort2Count.smt2")
                      (testing-file "tip2015/sort_NStoogeSort2Permutes.smt2"))
                   (tip2015/list_SelectPermutations.smt2isPermutation
                    tip2015/sort_NStoogeSort2Count.smt2nstoogesort2))

                  (,(begin
                      (testing-file "tip2015/heap_SortPermutes'.smt2")
                      (testing-file "tip2015/tree_sort_SortPermutes'.smt2"))
                   ,(list (quote |tip2015/heap_SortPermutes'.smt2isPermutation|)
                          (quote |tip2015/tree_sort_SortPermutes'.smt2tsort|)))))

                ("With constructors"
                 ((,(testing-file "tip2015/propositional_AndCommutative.smt2")
                   (custom-bool-converter
                    tip2015/propositional_AndCommutative.smt2valid
                    constructor-tip2015/propositional_AndCommutative.smt2&))

                  (,(begin
                      (testing-file "tip2015/propositional_AndCommutative.smt2")
                      (testing-file "tip2015/propositional_AndIdempotent.smt2"))
                   (tip2015/propositional_AndCommutative.smt2valid
                    constructor-tip2015/propositional_AndCommutative.smt2&
                    custom-bool-converter))

                  (,(begin
                      (testing-file "prod/prop_35.smt2")
                      (testing-file "isaplanner/prop_01.smt2")
                      (testing-file "tip2015/nat_pow_one.smt2"))
                   (prod/prop_35.smt2exp
                    constructor-isaplanner/prop_01.smt2S
                    constructor-isaplanner/prop_01.smt2Z
                    custom-bool-converter)))))))

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
              (range 1 10)))

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
              (all-theorem-deps)))

  (def-test-case "Conjecture-finding"
    (for-each (lambda (f)
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
                          conjectures))
              (theorem-files)))

  (def-test-case "Precision"
    (check-equal? (/ 1 10)
                  (precision (list->set '(a b c d e f g h i j))
                             (list->set '(j k l m n o p q r s t u v w x y z)))))

  (def-test-case "Recall"
    (check-equal? (/ 1 2)
                  (recall (list->set '(a b c d e f g h i j k l m))
                          (list->set '(a b c d e f g h i j k l m
                                       n o p q r s t u v w x y z)))))

  (def-test-case "Comparisons"
    (check-true  (lex<=? 'a       'b))
    (check-true  (lex<=? 'a       'a))
    (check-true  (lex<=? 'a       '()))
    (check-true  (lex<=? '()      '()))
    (check-true  (lex<=? '()      '(a)))
    (check-true  (lex<=? '(a)     '(b)))
    (check-true  (lex<=? '(a)     '(a)))
    (check-true  (lex<=? '(a b c) '(a c b)))

    (check-false (lex<=? 'b       'a))
    (check-false (lex<=? '()      'a))
    (check-false (lex<=? '(a)     '()))
    (check-false (lex<=? '(b)     '(a)))
    (check-false (lex<=? '(a c b) '(a b c))))

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
                 "Reject variables in the wrong order")

    (check-equal? (equation-from
                   (benchmark-file
                    (testing-file "grammars/packrat_unambigPackrat.smt2")))
                  '()
                  "Theorem which isn't equation doesn't get converted")

    (check-true (list? (map testing-file
                            '("grammars/packrat_unambigPackrat.smt2"
                              "isaplanner/prop_44.smt2"
                              "isaplanner/prop_01.smt2"
                              "isaplanner/prop_15.smt2")))
                "Files containing normal forms are included")
    (check-equal? (equation-from
                   (benchmark-file
                    (testing-file "isaplanner/prop_84.smt2")))

                  '((~=
                     (apply (apply (constant grammars/packrat_unambigPackrat.smt2append "unknown")
                                   (apply (apply (constant isaplanner/prop_44.smt2zip "unknown")
                                                 (apply (apply (constant isaplanner/prop_01.smt2take "unknown")
                                                               (apply (constant isaplanner/prop_15.smt2len "unknown")
                                                                      (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                                                        (variable 0 "(grammars/packrat_unambigPackrat.smt2list a)")))
                                          (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                            (apply (apply (constant isaplanner/prop_44.smt2zip "unknown")
                                          (apply (apply (constant isaplanner/prop_01.smt2drop "unknown")
                                                        (apply (constant isaplanner/prop_15.smt2len "unknown")
                                                               (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)")))
                                                 (variable 0 "(grammars/packrat_unambigPackrat.smt2list a)")))
                                   (variable 1 "(grammars/packrat_unambigPackrat.smt2list b)")))

                     (apply (apply (constant isaplanner/prop_44.smt2zip "unknown")
                                   (variable 0 "(grammars/packrat_unambigPackrat.smt2list a)"))
                            (apply (apply (constant grammars/packrat_unambigPackrat.smt2append "unknown")
                                          (variable 0 "(grammars/packrat_unambigPackrat.smt2list b)"))
                                   (variable 1 "(grammars/packrat_unambigPackrat.smt2list b)")))))

                  "Theorem which is equation gets converted")

    (for-each (lambda (f)
                (check-true (< (length (equation-from f)) 2)
                            "Extracting equations doesn't crash"))
              (theorem-files))

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
              (theorem-files)))

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
      (file->string (test-data "nat-simple-raw.json")))

    (check-true  (jsexpr? (string->jsexpr test-eqs)))
    (check-true  (list?   (string->jsexpr test-eqs)))
    (check-false (empty?  (string->jsexpr test-eqs)))

    (for-each (lambda (obj)
                (check-pred equation? obj))
              (parse-json-equations test-eqs)))

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
