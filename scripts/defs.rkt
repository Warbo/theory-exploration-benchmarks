#lang racket
(require racket/contract)
(require racket/contract/combinator)
(require racket/function)
(require racket/match)
(require racket/trace)
(require shell/pipeline)

(provide full-haskell-package)
(provide mk-defs)
(provide mk-final-defs)
(provide mk-signature)
(provide qualify-given)
(provide symbols-of-theorems)
(provide theorems-from-symbols)
(provide types-from-defs)

(define verbose #t)
(define (quiet)
  (set! verbose #f))
(define (log . args)
  (when verbose
    (apply eprintf args)))

;; Uses 'define/contract' during testing, and 'define' otherwise. Useful since
;; 'define/contract' can be very slow, e.g. checking every recursive call.
(define-syntax (define/test-contract stx)
  (syntax-case stx ()
    [(define/test-contract sig contract body ...)
     (if (and (getenv "PLT_TR_CONTRACTS") #t)
         #'(define/contract sig contract body ...)
         #'(define          sig          body ...))]))

(define benchmark-dir
  (or (getenv "BENCHMARKS")
      "No BENCHMARKS env var given"))

(define benchmark-file
  (curry string-append benchmark-dir "/"))

(define benchmark-files
  (curry map benchmark-file))

(define (symbols-in exp)
  (define native-symbols
    (list 'Int 'Bool '* '> 'mod 'and 'or 'xor 'iff 'ite 'true 'false 'not
          'implies 'distinct '@ '= '<= '- '+ '* 'div '=> 'as))

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

;; Extract the symbols used by a benchmark expression

(define (expression-constructors exp)
  (let* ((constructor-symbols (lambda (c)
           (match c
             [(cons name vars) (list name)]
             [_                (error "Unexpected constructor form")])))

         (constructors-from-def (lambda (given decs)
           (remove* (symbols-in given)
                    (symbols-in (foldl (lambda (dec got)
                                         (append got (match dec
                                                       [(cons type defs) (symbols-in (map constructor-symbols defs))]
                                                       [_                (error "Unexpected type definition")])))
                                       null
                                       decs))))))
    (match exp
      [(list 'declare-datatypes given decs) (constructors-from-def given decs)]
      [(cons a b)                           (append (expression-constructors a)
                                                    (expression-constructors b))]
      [_                                    null])))

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

(define (expression-symbols exp)
  (define (expression-destructors exp)
    (let* ((destructor-symbols (lambda (c)
                                 (match c
                                   [(cons name vars) (map car vars)]
                                   [_                (error "Unexpected destructor form")])))

           (destructors-from-def (lambda (decs)
                                   (symbols-in (foldl (lambda (dec got)
                                                        (append got (match dec
                                                                      [(cons type defs) (symbols-in (map destructor-symbols defs))]
                                                                      [_                (error "Unexpected type def")])))
                                                      null
                                                      decs)))))
      (match exp
        [(list 'declare-datatypes given decs) (destructors-from-def decs)]
        [(cons a b)                           (append (expression-destructors a)
                                                      (expression-destructors b))]
        [_                                    null])))

  (define (expression-funs exp)
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
      [_                                            null]))

  (remove* (expression-types exp)
           (append (expression-constructors exp)
                   (expression-destructors  exp)
                   (expression-funs         exp))))

(define (qualify name expr)
  (foldl (lambda (sym x)
           (replace-in sym
                       (string->symbol (string-append name
                                                      (as-str sym)
                                                      "-sentinel"))
                       x))
         (prefix-locals expr)
         (symbols-in (append (expression-symbols expr)
                             (expression-types   expr)))))

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
    (define (prefix-local s)
      (string->symbol (string-append "local-" (as-str s))))

    (if (empty? locals)
        expr
        (prefix-all (cdr locals)
                    (replace-in (car locals)
                                (prefix-local (car locals))
                                expr))))

  (prefix-all (locals-in expr) expr))

(define (replace-in old replacement expr)
  (if (equal? old expr)
      replacement
      (match expr
        [(cons a b) (cons (replace-in old replacement a)
                          (replace-in old replacement b))]
        [_          expr])))

(define (replace-all reps expr)
  (if (empty? reps)
      expr
      (replace-all (cdr reps)
                   (replace-in (first  (first reps))
                               (second (first reps))
                               expr))))

(define (format-symbols syms)
  (if (null? syms)
      ""
      (format "~a\n~a" (car syms) (format-symbols (cdr syms)))))

(define (show x)
  (displayln (format-symbols x)))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define theorem-files
  ;; Directory traversal is expensive; if we have to do it, memoise the result
  (let ([result #f])
    (lambda ()
      (when (equal? #f result)
        (set! result
              (map path->string
                   (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
                      (sequence->list (in-directory benchmark-dir))))))
      result)))

(define (symbols-of-theorem path)
  (benchmark-symbols (read-benchmark (file->string path))))

(define (files-with given)
  (filter (lambda (path)
            (member given (map symbol->string (symbols-of-theorem path))))
          (theorem-files)))

(define (read-benchmark x)
  (let* ([content (string-append "(\n" x "\n)")])
    (with-input-from-string content
      read)))

(define (lowercase-names expr)
  "Return the names of all functions defined in the given expr, including
   destructors"
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

(define (uppercase-names expr)
  "Return the names of all types and constructors defined in the given expr"
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

(define (benchmark-symbols expr)
  (remove-duplicates (expression-symbols expr)))

(define/test-contract (norm expr)
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

    [_ expr]))

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

(define (inc-name pre n)
  (string->symbol (string-append pre (number->string (+ 1 n)))))

(define         var-prefix "normalise-var-")

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

(define (names-in defs)
  (append (lowercase-names defs)
          (uppercase-names defs)))

(define (tag-constructors x)
  ;; Tag constructors with 'CONSTRUCTOR' to disambiguate
  (foldl (lambda (c y)
           (replace-in c (prefix-name c "CONSTRUCTOR") y))
         x
         (expression-constructors x)))

(define (tag-types x)
  ;; Tag types with 'TYPE' to disambiguate
  (foldl (lambda (t y)
           (if (symbol? t)
               (replace-in t (prefix-name t "TYPE") y)
               y))
         x
         (expression-types x)))

(define (prefix-name n p)
  (string->symbol (string-append p (symbol->string n))))

(define (concat-map f xs)
  (apply append (map f xs)))

(define (trim lst)
  (filter (lambda (x)
            (and (not (equal? (first x) 'assert-not))
                 (not (equal? x '(check-sat)))))
          lst))

(define (qual-all given-files)
  (define (take-from-end n lst)
    (reverse (take (reverse lst) n)))

  (define given-contents
    (map (lambda (pth)
           (list (string-replace (string-join (take-from-end 2 (string-split pth "/"))
                                              "/")
                                 "'" "_tick_")
                 (read-benchmark (file->string pth))))
         given-files))

  (define qualified-contents
    (map (lambda (name-content)
           (qualify (first name-content) (second name-content)))
         given-contents))

  (trim (apply append qualified-contents)))

(define (mk-defs)
  (show (mk-defs-s (port->lines (current-input-port)))))

(define (mk-defs-s given-files)
  (norm-defs (qual-all given-files)))

;; Combine all definitions in files given on stdin

(define (non-empty? x)
  (not (empty? x)))

;; Check each function declaration syntax

(define (ss-eq? x y)
  (cond ([symbol? x]
         (ss-eq? (symbol->string x) y))
        ([symbol? y]
         (ss-eq?  x (symbol->string y)))
        (#t
         (equal? x y))))

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
    [(cons h t)                        (append (find-sub-exprs f h)
                                               (find-sub-exprs f t))    ]
    [_                                 '()                              ]))

(define (as-str x)
  (if (string? x)
      x
      (symbol->string x)))

(define (any-of f xs)
  (match xs
    [(cons a b) (or (f a) (any-of f b))]
    [_          #f]))

(define (all-of f xs)
  (match xs
    [(cons a b) (and (f a) (all-of f b))]
    [_          #t]))

(define/test-contract (get-def-s name exprs)
  (-> symbol? any/c any/c)

  (define/test-contract (defs-from sym exp)
    (-> symbol? any/c any/c)

    (match exp
      [(list 'declare-datatypes given decs) (if (member sym (names-in exp))
                                                (list exp)
                                                '())]
      [(cons a b)                           (append (defs-from sym a)
                                                    (defs-from sym b))]
      [_                                    null]))

  (remove-duplicates (append (find-sub-exprs name exprs)
                             (foldl (lambda (expr rest)
                                      (append (defs-from name expr) rest))
                                    '()
                                    exprs))))

(define (get-def name str)
  (get-def-s name (read-benchmark str)))

(define (rec-names exprs)
  (names-in exprs))

(define (remove-suffix x)
  ;; Strip "-sentinel"
  (string->symbol
   (string-reverse
    (substring (string-reverse (symbol->string x))
               9))))

(define (remove-suffices x)
  ;; Removes '-sentinel' suffices. Do this after all other string-based
  ;; transformations, since the sentinels prevent us messing with, say, the
  ;; symbol "plus2", when we only wanted to change the symbol "plus"
  (read-benchmark (string-replace (format-symbols x)
                                  "-sentinel"
                                  "")))

(define (add-constructor-funcs x)
  ;; Adding function for each constructor
  (let* ([consts (expression-constructors x)])
    (define (func-for x c)
      (define (arg-decs-for x)
        ;; Look through x for any definitions of c, and return its argument list
        (define (arg-decs-for-ty x)
          (define (arg-decs-for-con x)
            (match x
              [(list name)      (if (equal? name c) (list '())  '())]
              [(cons name args) (if (equal? name c) (list args) '())]))

          (concat-map arg-decs-for-con (cdr x)))

        (match x
          [(list 'declare-datatypes _ decs) (concat-map arg-decs-for-ty decs)]
          [(cons h t) (append (arg-decs-for h)
                              (arg-decs-for t))]
          [_ '()]))

      (define arg-decs
        (car (arg-decs-for x)))

      (define (constructor-type x)
        (define (constructor-type-ty dec)
          (concat-map (lambda (con)
                        (if (equal? (car con) c)
                            (list (car dec))
                            '()))
                      (cdr dec)))

        (match x
          [(list 'declare-datatypes _ decs) (concat-map constructor-type-ty decs)]
          [(cons a b) (append (constructor-type a)
                              (constructor-type b))]
          [_ '()]))

      (prefix-locals
       `(define-fun
          ,(prefix-name c "constructor")
          ,arg-decs
          ,(car (constructor-type x))
          ,(if (empty? arg-decs)
               c
               (cons c (map car (car (arg-decs-for x))))))))

    (append x (map (curry func-for x) consts))))

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

(define (decode-name name)
  (define no-global
    (substring (symbol->string name) 6))

  (string->symbol (decode16 no-global)))

(define (encode-names expr)
  ;; Replace function names with "globalXXX" where "XXX" is a hex encoding of
  ;; the name
  (define lower-encoded
    (foldl (lambda (name e)
             (replace-in name (encode-lower-name name) e))
           expr
           (lowercase-names expr)))

  ;; Replace type and constructor names with "GlobalXXX" where "XXX" is a hex
  ;; encoding of the name
  (foldl (lambda (name e)
           (replace-in name (encode-upper-name name) e))
         lower-encoded
         (uppercase-names expr)))

(define (encode-lower-name name)
  (string->symbol (string-append "global"
                                 (encode16 (symbol->string name)))))

(define (encode-upper-name name)
  (string->symbol (string-append "Global"
                                 (encode16 (symbol->string name)))))

(define (prepare x)
  (define (add-check-sat x)
    ;; Add '(check-sat)' as the last line to appease tip-tools
    (append x '((check-sat))))

  ;(tag-types
    ;(tag-constructors
      ;(add-constructor-funcs
  (add-check-sat (encode-names (remove-suffices x))))

(define (zip xs ys)
  (if (empty? xs)
      null
      (if (empty? ys)
          null
          (cons (list (car xs) (car ys))
                (zip  (cdr xs) (cdr ys))))))

(define (symbol<=? x y)
  (string<=? (symbol->string x)
             (symbol->string y)))

(define (symbol<? x y)
  (string<? (symbol->string x)
            (symbol->string y)))

(define (definition? x)
  (and (not (empty? (names-in x)))
       (not (any-of (lambda (sub-expr)
                      (not (empty? (names-in sub-expr))))
                    x))))

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

  (define/test-contract (choose-smallest so-far)
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
    (-> (and/c (*list/c (list/c class? normalised-def?))

               ;; Each name in so-far is defined in exprs
               (lambda (so-far)
                 (all-of (lambda (name)
                           (member name (names-in exprs)))
                         (concat-map (lambda (known)
                                       (apply append (first known)))
                                     so-far)))
               ;; We can find a definition for each name in so-far
               (lambda (so-far)
                 (all-of (lambda (name)
                           (define defs
                             (get-def-s name exprs))

                           (or (equal? 1 (length defs))
                               (raise-user-error
                                'so-far
                                "Expected all names in so-far to have one definition in exprs, yet for '~a' we found the definitions:\n~a\nThe value of exprs is:\n~a"
                                name
                                defs
                                exprs)))
                         (concat-map (lambda (known)
                                       (apply append (first known)))
                                     so-far)))

               ;; Each class's names come from alpha-equivalent definitions
               (*list/c (flat-contract-with-explanation
                         (lambda (known)
                           (all-of (lambda (name)
                                     (define def
                                       (first (get-def-s name exprs)))
                                     (or (equal? (norm def) (second known))
                                         (raise-user-error
                                          'so-far
                                          "Expected list of '(class def)' pairs, where the names in 'class' have definitions alpha-equivalent to 'def', yet for 'def' of\n~a\nthe name '~a' has definition:\n~a\nwhich normalises to:\n~a\n"
                                          (second known)
                                          name
                                          def
                                          (norm def))))
                                   (apply append (first known)))))))
        (*list/c (and/c (list/c symbol? symbol?)
                        (lambda (pair)
                          (symbol<? (second pair) (first pair)))
                        (lambda (pair)
                          (and (member (first  pair) (names-in exprs))
                               (member (second pair) (names-in exprs))))
                        (lambda (pair)
                          (equal? (norm (get-def-s (first  pair) exprs))
                                  (norm (get-def-s (second pair) exprs)))))))

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
    (define/test-contract result
      (*list/c (and/c (list/c symbol? symbol?)
                      (lambda (pair)
                        (equal? (norm (get-def-s (first  pair) exprs))
                                (norm (get-def-s (second pair) exprs))))))

      (concat-map pick-replacements all-classes))

    result)

  (define (mk-output expr so-far)
    (let* ([norm-line (norm     expr)]
           [names     (names-in expr)]
           ;; Any existing alpha-equivalent definitions:
           ;;   '(((name1 name2 ...) expr) ...)
           [existing  (filter (lambda (x)
                                (equal? norm-line (second x)))
                              so-far)])
      (if (empty? existing)
          ;; This expr isn't redundant, associate its names with its normal form
          (append so-far (list (list (list names) norm-line)))

          ;; This expr is redundant, associate its names with their equivalents
          (map (lambda (known)
                 (if (equal? (second known) norm-line)
                     (list (cons names (first known))
                           norm-line)
                     known))
               so-far))))

  (define (remove-redundancies exprs so-far)
    (if (empty? exprs)
        so-far
        (remove-redundancies (cdr exprs)
                             (mk-output (first exprs) so-far))))

  (choose-smallest (remove-redundancies exprs null)))

(define (set-equal? x y)
  (equal? (list->set x) (list->set y)))

(define (symbols-of-theorems-s expr)
  (filter (lambda (s)
            (not (member s '(true-sentinel
                             false-sentinel
                             or-sentinel
                             ite-sentinel))))
          (benchmark-symbols expr)))

(define (symbols-of-theorems)
  (displayln (string-join (map ~a (symbols-of-theorems-s
                                   (read-benchmark
                                    (port->string (current-input-port)))))
                          "\n")))

(define (pipe s f)
  (define o (open-output-string))
  (parameterize ([current-input-port  (open-input-string s)]
                 [current-output-port o])
    (f))
  (get-output-string o))

(define (qualify-given)
  (define given
    (read-benchmark (port->string (current-input-port))))

  (define name
    (string-replace (getenv "NAME") "'" "_tick_"))

  (show (qualify name given)))

(define (theorems-from-symbols-s given-symbols)
  (define (acceptable-theorem thm-path)
    (null? (remove* given-symbols
                    (symbols-of-theorem thm-path))))

  (filter acceptable-theorem (theorem-files)))

(define (theorems-from-symbols)
  (show (theorems-from-symbols-s (port->lines (current-input-port)))))

(define (replace-strings str reps)
  ;; For each (src dst) in reps, replaces src with dst in str
  (foldl (lambda (pair so-far)
           (string-replace so-far (as-str (first  pair))
                                  (as-str (second pair))))
         str
         reps))

(define/test-contract (norm-defs exprs)
  (-> (*list/c definition?)
      (*list/c definition?))

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

    (foldl (lambda (rep exprs)
             (replace-in (first rep) (second rep) exprs))
           exprs
           redundancies))

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

  (log "Stripped ~a redundancies\n" (- (length exprs) (length stripped)))
  (if (equal? exprs stripped)
      stripped
      (norm-defs stripped)))

(define (defs-to-sig x)
  (mk-signature-s (format-symbols (mk-final-defs-s (string-split x "\n")))))

(define (mk-final-defs)
  (show (mk-final-defs-s (port->lines (current-input-port)))))

(define (mk-final-defs-s given-files)
  (prepare (mk-defs-s given-files)))

(define temp-file-prefix
  "tebenchmarktemp")

(define (with-temp-file data proc)
  (let* ([f      (make-temporary-file (string-append temp-file-prefix "~a"))]
         [result void])
    (display-to-file data f #:exists 'replace)
    (set! result (proc f))
    (delete-file f)
    result))

(define (dump x) (write x (current-error-port)))

(define (mk-signature-s input)
  (with-temp-file input (lambda (f)
                          (run-pipeline/out `(tip ,f --haskell-spec)))))

(define (mk-signature)
  (display (mk-signature-s (port->string (current-input-port)))))

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

(define (symbols-from-file f)
  (symbols-of-theorems-s (read-benchmark (file->string f))))

(define (function-name-to-haskell n)
  n)

(define (types-from-defs)
  (show (symbols-in
         (remove-duplicates
          (symbols-in
           (expression-types
            (read-benchmark (port->string (current-input-port)))))))))

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

(define (full-haskell-package)
  (full-haskell-package-s (port->string (current-input-port))
                          (getenv "OUT_DIR")))

;; Everything from here is tests
(module+ test
  (require rackunit)

  ;; Don't give progress information during tests
  (quiet)

  ;; Examples used for tests
  (define nat-def      '(declare-datatypes () ((Nat (Z) (S (p Nat))))))

  (define constructorZ '(define-fun constructorZ ()              Nat Z))

  (define constructorS '(define-fun constructorS ((local-p Nat)) Nat (S local-p)))

  (define redundancies `(,constructorZ
                         ,constructorS
                         (define-fun redundantZ1 () Nat Z)
                         (define-fun redundantZ2 () Nat Z)
                         (define-fun redundantZ3 () Nat Z)))

  (check-equal? (symbols-in '(lambda ((local1 Nat) (local2 (List Nat)))
                               (free1 local1)))
                '(free1))

  (check-equal? (concat-map (lambda (x) (list x x x))
                            '(fee fi fo fum))
                '(fee fee fee fi fi fi fo fo fo fum fum fum))

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
                   (filter non-empty? (rec-names (list expr))))
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

  (check-equal? (find-sub-exprs "constructorZ"
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

  (test-case "Real symbols qualified"
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

  (test-case "No alpha-equivalent duplicates in result"
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

  (check-equal? (get-def-s 'constructorZ redundancies)
                (list constructorZ))

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

  (define test-benchmark-files
    (take (shuffle (theorem-files)) 10))

  (test-case "Smallest name chosen"
    ;; The names which appear after normalising should be the first,
    ;; lexicographically, from each alpha-equivalent group

    ;; Collect together some definitions, which include some alpha-equivalent
    ;; redundancies
    (define files (append test-files
                          test-benchmark-files
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

  (check-equal? (remove-suffices qualified-example)
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

  (test-case "Find redundancies"
    ;; Check known expression
    (check-equal? (list->set (find-redundancies redundancies))
                  (list->set '((redundantZ1 constructorZ)
                               (redundantZ2 constructorZ)
                               (redundantZ3 constructorZ))))

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

  (test-case "Normalise"
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

  (define (string-to-haskell val)
    ;; mk-final-defs takes in filenames, so it can qualify names. This makes
    ;; and cleans up temporary files for testing.

    ;; Note: We make a file in a directory, to avoid problems if tmpdir begins
    ;; with a number (e.g. '/var/run/user/1000'); otherwise qualified variable
    ;; names would be invalid
    (in-temp-dir (lambda (dir)
                   (define temp-file (string-append (path->string dir)
                                                    "/test.smt2"))

                   (display-to-file val temp-file)

                   (let* ([result (defs-to-sig temp-file)])
                     (delete-file temp-file)
                     result))))

  (test-case "Form"
    (define form
      '(declare-datatypes ()
         ((Form (& (&_0 Form) (&_1 Form))
                (Not (Not_0 Form))
                (Var (Var_0 Int))))))

    (define prepared
      (prepare form))

    (with-check-info
     (('prepared prepared)
      ('message  "Prepared 'Form' input defines a datatype"))
     (check-not-equal? #f
                       (member 'declare-datatypes (flatten prepared))))

    (define sig (string-to-haskell form))

    (let ([data-found (regexp-match? "data[ ]+Global[0-9a-f]+[ ]+="
                                     (string-replace sig "\n" ""))])
      (with-check-info
       (('data-found data-found)
        ('sig        sig)
        ('message    "'Form' datatype appears in signature"))
       (check-true data-found))))

  (test-case "Mutual recursion"
    (define mut '(define-funs-rec
                   ((models  ((x Bool)
                              (y Int))
                             Bool)
                    (models2 ((q Bool)
                              (x Int))
                             Bool)
                    (models5 ((q Bool)
                              (x Int)
                              (y Int))
                             Bool))

                   ((ite x
                         (models2 (models x y) y)
                         (models5 (models x y) y y))

                    (ite q
                         (models5 (models q x) x x)
                         (models2 (models q x) x))

                    (ite q
                         (models2 q x)
                         (models5 q x y)))))

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

  (test-case "Single files"
    (define files (map (lambda (suf)
                         (benchmark-file suf))
                       '("isaplanner/prop_54.smt2"
                         "tip2015/propositional_AndIdempotent.smt2"
                         "tip2015/propositional_AndCommutative.smt2"
                         "tip2015/mccarthy91_M2.smt2"
                         "isaplanner/prop_36.smt2"
                         "tip2015/sort_MSortTDPermutes.smt2"
                         "tip2015/tree_sort_SortPermutes'.smt2"
                         "tip2015/sort_StoogeSort2Permutes.smt2"
                         "tip2015/sort_StoogeSortPermutes.smt2"
                         "tip2015/polyrec_seq_index.smt2"
                         "tip2015/sort_QSortPermutes.smt2")))

    (for-each (lambda (f)
                (define sig
                  (defs-to-sig f))
                (check-true (string-contains? sig "QuickSpec")))
              files))

  (test-case "Multiple files"
    (define files
      (string-join (benchmark-files '("tip2015/tree_SwapAB.smt2"
                                      "tip2015/list_z_count_nub.smt2"))
                   "\n"))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('message "Local variables renamed")
      ('sig     sig))
     (check-true (string-contains? sig "local"))))

  (test-case "Random files"
    (define files
      (string-join test-benchmark-files "\n"))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('files   files)
      ('sig     sig)
      ('message "Made Haskell for random files"))
     (check-true (string-contains? sig "QuickSpec"))))

  (check-equal? (with-temp-file "foo\nbar\nbaz"
                                file->string)
                "foo\nbar\nbaz")

  (let ([a (getenv "HOME")]
        [b (parameterize-env '([#"HOME" #"foo"])
                             (lambda () (getenv "HOME")))]
        [c (getenv "HOME")])
    (check-equal? a c)
    (check-equal? b "foo"))

  (test-case "Module tests"
    (define files
      (string-join test-benchmark-files "\n"))

    (in-temp-dir
     (lambda (dir)
       (define out-dir (path->string dir))
       (parameterize-env `([#"FILES"   ,(string->bytes/utf-8 files)]
                           [#"OUT_DIR" ,(string->bytes/utf-8 out-dir)])
         (lambda ()
           (full-haskell-package-s
            (format-symbols (mk-final-defs-s test-benchmark-files))
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
           (parameterize ([current-directory out-dir])
             (run-pipeline/out '(cabal configure))

             (define out
               (run-pipeline/out '(echo -e "import A\n:browse")
                                 '(cabal repl -v0)))

             ;; If the import fails, we're stuck with the Prelude, which contains classes
             (with-check-info
              (('out     out)
               ('message "Module imported successfully"))
              (check-false (string-contains? out "class Functor")))))))))

  (define regressions
    (benchmark-files '("tip2015/list_elem_map.smt2"
                       "tip2015/propositional_AndCommutative.smt2")))

  (for-each (lambda (file)
    (define name  (last (string-split file "/")))
    (define name2 (string-replace name "'" "_tick_"))
    (define syms  (symbols-of-theorems-s
                   (qualify name2
                            (read-benchmark (file->string file)))))

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
    (append regressions test-benchmark-files))

  (in-temp-dir
   (lambda (out-dir)
     (parameterize-env `([#"FILES"   ,(string->bytes/utf-8
                                       (string-join test-benchmark-files "\n"))]
                         [#"OUT_DIR" ,(string->bytes/utf-8
                                       (path->string out-dir))]
                         [#"HOME"    ,(string->bytes/utf-8
                                       (path->string out-dir))])
       (lambda ()
         (full-haskell-package-s (format-symbols (mk-final-defs-s test-benchmark-files))
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
                                          "class Functor")))))))

  (define (names-match src expr expect)
    (define names (rec-names expr))

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

  (test-case "Symbol lookup"
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

      (should-have syms 'constructor '(Pos Neg Z S P N))

      (should-have syms 'destructor  '(p P_0 N_0))

      (should-have syms 'function '(toInteger sign plus2 opposite
                                    timesSign mult minus plus absVal
                                    times))

      (should-not-have syms 'type '(Nat     Nat-sentinel
                                    Sign    Sign-sentinel
                                    Integer Integer-sentinel
                                    =>      =>-sentinel))

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
                                        check-sat         check-sat-sentinel))

      (define theorems
        (theorems-from-symbols-s syms))

      (with-check-info
       (('theorems theorems)
        ('f        f)
        ('syms     syms)
        ('message  "Theorem allowed by its own symbols"))
       (check-true (contains theorems f))))

    (let* ([f    (benchmark-file "tip2015/list_PairEvens.smt2")]
           [syms (symbols-from-file f)])
      (should-not-have syms 'higher-order-type '(=> =>-sentinel)))

    (let* ([f    (benchmark-file "tip2015/propositional_AndCommutative.smt2")]
           [syms (symbols-from-file f)])
      (should-have syms 'function '(or2))))

  (let ([f (benchmark-file "tip2015/nat_alt_mul_comm.smt2")])
    (check-equal? (string-trim (pipe (file->string f) types-from-defs))
                  "Nat"))

  (test-case "Name extraction"
    (check-equal?
     (lowercase-names
      (read-benchmark
       (file->string (benchmark-file "tip2015/sort_NStoogeSort2Permutes.smt2"))))
     '(head tail first second p zelem zdelete twoThirds third take sort2 null
       zisPermutation length drop splitAt append nstooge2sort2 nstoogesort2
       nstooge2sort1))

    (check-equal?
     (uppercase-names
      (read-benchmark
       (file->string (benchmark-file "tip2015/sort_NStoogeSort2Permutes.smt2"))))
     '(list nil cons Pair Pair2 Nat Z S)))

  ;; When TIP translates from its smtlib-like format to Haskell, it performs a
  ;; renaming step, to ensure that all names are valid Haskell identifiers. We
  ;; need to ensure that the names we produce don't get altered by this step.
  (test-case "Name preservation"
    (define test-benchmark-defs
      (mk-final-defs-s test-benchmark-files))

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
              test-benchmark-upper-names)))
