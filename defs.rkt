#lang racket
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

;; Examples used for tests
(define nat-def      '(declare-datatypes () ((Nat (Z) (S (p Nat))))))

(define constructorZ '(define-fun constructorZ ()              Nat Z))

(define constructorS '(define-fun constructorS ((local-p Nat)) Nat (S local-p)))

(define redundancies `(,constructorZ
                       ,constructorS
                       (define-fun redundantZ1 () Nat Z)
                       (define-fun redundantZ2 () Nat Z)
                       (define-fun redundantZ3 () Nat Z)))

(define benchmark-dir
  "modules/tip-benchmarks/benchmarks")

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
                       (string-append name (as-str sym) "-sentinel")
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

(define (format-symbols syms)
  (if (null? syms)
      ""
      (format "~a\n~a" (car syms) (format-symbols (cdr syms)))))

(define (show x)
  (displayln (format-symbols x)))

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

(define (benchmark-symbols expr)
  (remove-duplicates (expression-symbols expr)))

(define (norm expr)
  (define (norm-let bindings body)
    (if (empty? bindings)
        (list bindings (norm body))
        (let* ([binding   (first  bindings)]
               [name      (first  binding)]
               [value     (second binding)]
               [new-value (norm value)]
               [rec       (norm-let (cdr bindings) body)]
               [new-binds (first  rec)]
               [new-body  (second rec)]
               [new-name  (next-var (list new-body new-value new-binds))])
          (list (cons (list new-name new-value) new-binds)
                (replace-in name new-name new-body)))))

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

  (define (norm-type-params ps decs)
    (if (empty? ps)
        (list ps decs)
        (let* ([rec  (norm-type-params (cdr ps) decs)]
               [name (inc-name var-prefix (max-name var-prefix rec))])
          (list (cons name (first rec))
                (replace-in (car ps)
                            name
                            (second rec))))))

  (define (norm-types decs)
    (define (norm-type dec rest)
      (define (norm-constructors cs rest)
        (define (norm-constructor c rest)
          (define constructor-prefix
            "normalise-constructor-")

          (define (norm-destructors ds rest)
            (define  destructor-prefix "normalise-destructor-")

            (if (empty? ds)
                ds
                (let* ([rec  (norm-destructors (cdr ds) rest)]
                       [name (inc-name destructor-prefix (max-name destructor-prefix (list rest rec)))])
                  (cons (cons name (cdr (car ds))) rec))))

          (let ([name (inc-name constructor-prefix (max-name constructor-prefix rest))])
            (cons name (norm-destructors (cdr c) rest))))

        (if (empty? cs)
            cs
            (let ([rec (norm-constructors (cdr cs) rest)])
              (cons (norm-constructor (car cs) (list rest rec)) rec))))

      (let* ([type-prefix "defining-type-"]
             [name (inc-name type-prefix (max-name type-prefix rest))]
             [cs   (norm-constructors (cdr dec) rest)])
        (cons name (replace-in (car dec) name cs))))

    (if (empty? decs)
        decs
        (let ([rec (norm-types (cdr decs))])
          (cons (norm-type (car decs) rec) rec))))

  (match expr
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
     (let* ([norm-decs (norm-types decs)]
            [rec       (norm-type-params given norm-decs)])
       (list 'declare-datatypes (car rec) (cdr rec)))]

    [  (list 'case pat body)
     (let* ([norm-body  (norm body)]
            [rec        (match pat
                          [(list con)    (list pat norm-body)]
                          [(cons con ps) (match (foldl (lambda (x y)
                                                         (let ([name (next-var y)])
                                                           (list (cons name (first y))
                                                                 (replace-in x name (second y)))))
                                                       (list '() norm-body)
                                                       ps)
                                           [(list norm-ps norm-body2)
                                            (list (cons con norm-ps) norm-body2)])]
                          [_             (list pat norm-body)])])
       (cons 'case rec))]

    [(list 'lambda args body)
     (let ([rec (norm-func args body)])
       (list 'lambda (first rec) (second rec)))]

    [(list 'let bindings body)
     (let* ([rec (norm-let bindings (norm body))])
       (list 'let (first rec) (second rec)))]

    [(cons a b) (cons (norm a) (norm b))]

    [_ expr]))

(define (norm-func args body)
  (if (empty? args)
      (list args (norm body))
      (let* ([arg (car args)]
             [rec (norm-func (cdr args) body)]
             [v   (next-var rec)])
        (list (cons (cons v (cdr arg)) (first rec))
              (replace-in (car arg) v (second rec))))))

(define norm-func-prefix "defining-function-")
(define norm-func-1 (string->symbol (string-append norm-func-prefix "1")))

(define (next-var expr)
  (string->symbol
   (string-append var-prefix
                  (number->string (+ 1 (var-num (max-var expr)))))))

(define (max-var expr)
  (if (is-var? expr)
      expr
      (match expr
        [(cons a b) (let ([ma (max-var a)]
                          [mb (max-var b)])
                      (if (var-lt? ma mb) mb ma))]
        [_          (string->symbol (string-append var-prefix "0"))])))

(define (is-var? v)
  (if (symbol? v)
      (string-prefix? (symbol->string v) var-prefix)
      #f))

(define (var-lt? x y)
  (< (var-num x) (var-num y)))

(define (var-num x)
  (string->number (substring (symbol->string x) (string-length var-prefix))))

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
  (match defs
    [(list 'define-funs-rec decs defs)            (map car decs)]
    [(list 'define-fun
           (list 'par p
                 (list name args return body)))   (list name)]
    [(list 'define-fun name args return body)     (list name)]
    [(list 'define-fun-rec
           (list 'par p
                 (list name args return body)))   (list name)]
    [(list 'define-fun-rec name args return body) (list name)]
    [(list 'declare-datatypes given decs)         (type-names decs)]
    [(cons a b)                                   (append (names-in a) (names-in b))]
    [_                                            null]))

(define (type-names decs)
  (concat-map (lambda (dec)
                (cons (car dec) ; type name
                      (concat-map (lambda (con)
                                    (cons (car con) ; constructor name
                                          (map car (cdr con)))) ; destructor names
                                  (cdr dec))))
              decs))

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

(define (get-def-s name exprs)
  (define (defs-from sym exp)
    (define (find-defs sym given ty-decs)
      (define (any-of f xs)
        (match xs
          [(cons a b) (or (f a) (any-of f b))]
          [_          #f]))

      (map (lambda (dec) (list 'declare-datatypes given (list dec)))
           (filter (lambda (ty-dec)
                     (any-of (lambda (con-dec)
                               (or (equal? (symbol->string (car con-dec))
                                           sym)
                                   (any-of (lambda (des-dec)
                                             (equal? (symbol->string (car des-dec))
                                                     sym))
                                           (cdr con-dec))))
                             (cdr ty-dec)))
                   ty-decs)))

    (match exp
      [(list 'declare-datatypes given decs) (find-defs sym given decs)]
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

(define (remove-suffices x)
  ;; Removes '-sentinel' suffices. Do this after all other string-based
  ;; transformations, since the sentinels prevent us messing with, say, the
  ;; symbol "plus2", when we only wanted to change the symbol "plus"
  (read-benchmark (string-replace (format-symbols x)
                                  "-sentinel"
                                  "")))

(define (name-replacements-for x)
  ;; Unqualify any names which only have one definition
  (define nr-names (rec-names x))

  (foldl (lambda (sym rest)
           (define name (~a sym))
           (if (string-contains? name ".smt2")
               (let* ([unsent (substring name
                                         0
                                         (- (string-length name)
                                            (string-length "-sentinel")))]
                      [unqual (string-join (cdr (string-split unsent ".smt2"))
                                           ".smt2")]
                      [count  (- (length (string-split (format-symbols nr-names)
                                                       (string-append ".smt2"
                                                                      unqual
                                                                      "-sentinel")))
                                 1)])
                 (if (equal? count 1)
                     (cons (list name unqual) rest)
                     rest))
               rest))
         '()
         nr-names))

(define (remove-prefices x)
  ;; Removes unambiguous filename prefices
  (read-benchmark (foldl (lambda (rep str)
                           (string-replace str (first rep) (second rep)))
                         (format-symbols x)
                         (name-replacements-for x))))

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

(define (prepare x)
  (define (add-check-sat x)
    ;; Add '(check-sat)' as the last line to appease tip-tools
    (append x '((check-sat))))

  ;(tag-types
    ;(tag-constructors
      ;(add-constructor-funcs
  (add-check-sat (remove-suffices (remove-prefices x))))

(define (find-redundancies exprs)
  (define (mk-output expr so-far name-replacements)
    (define (zip xs ys)
      (if (empty? xs)
          null
          (if (empty? ys)
              null
              (cons (list (car xs) (car ys))
                    (zip  (cdr xs) (cdr ys))))))

    (let* ([norm-line (norm     expr)]
           [names     (names-in expr)]
           [existing  (filter (lambda (x)
                                (equal? norm-line (second x)))
                              so-far)])
      (if (empty? existing)
          (list (cons (list names norm-line) so-far)
                name-replacements)
          (list so-far
                (append (zip names (first (car existing)))
                        name-replacements)))))

  (define (remove-redundancies exprs so-far name-replacements)
    (if (empty? exprs)
        name-replacements
        (let* ([result (mk-output (first exprs) so-far name-replacements)]
               [new-sf (first  result)]
               [new-nr (second result)])
          (remove-redundancies (cdr exprs) new-sf new-nr))))

  (remove-redundancies exprs null null))

(define (set-equal? x y p)
  (equal? (sort x p) (sort y p)))

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

(define (norm-defs exprs)
  (define redundancies (find-redundancies exprs))
  (define replacements (map first redundancies))
  (define stripped
    (foldl (lambda (expr result)
             (let ([keep      #t]
                   [def-names (names-in expr)])
               (for-each (lambda (def-name)
                           (when (member def-name replacements)
                             (set! keep #f)))
                         def-names)
               (if keep
                   (append result (list expr))
                   result)))
           '()
           exprs))

  (define norm
    (read-benchmark (replace-strings (format-symbols stripped)
                                     (map (curry map ~a) redundancies))))

  (if (equal? exprs norm)
      norm
      (norm-defs norm)))

(define (defs-to-sig x)
  (mk-signature-s (format-symbols (mk-final-defs-s (string-split x "\n")))))

(define (mk-final-defs)
  (show (mk-final-defs-s (port->lines (current-input-port)))))

(define (mk-final-defs-s given-files)
  (prepare (mk-defs-s given-files)))

(define (with-temp-file data proc)
  (let* ([f      (make-temporary-file "te-benchmark-temp-~a")]
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
    (string-append benchmark-dir "/grammars/simp_expr_unambig3.smt2"))

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
         [defs (mk-defs-s (list (path->string (build-path (current-directory)
                                                          benchmark-dir
                                                          file))))])
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
                       "tip2015/sort_StoogeSort2IsSort.smt2")))

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
           [s (format-symbols (symbols-of-theorems-s q))])

      (check-true (string-contains? s "or2-sentinel")
                  "Found an or2 symbol")

      (check-false (member "or2-sentinel" (string-split s "\n"))
                   "or2 symbol is qualified")

      (let* ([d (format-symbols (mk-defs-s fs))]
             [s (format-symbols (symbols-of-theorems-s (read-benchmark d)))])
         (check-true (string-contains? s "or2-sentinel")
                     "Found 'or2' symbol"))))

  (define subset '("grammars/simp_expr_unambig1.smt2append-sentinel"
                   "grammars/simp_expr_unambig1.smt2lin-sentinel"
                   "grammars/simp_expr_unambig4.smt2nil-sentinel"
                   "grammars/simp_expr_unambig4.smt2cons-sentinel"
                   "grammars/simp_expr_unambig4.smt2C-sentinel"
                   "grammars/simp_expr_unambig4.smt2D-sentinel"
                   "grammars/simp_expr_unambig4.smt2X-sentinel"
                   "grammars/simp_expr_unambig4.smt2Y-sentinel"
                   "grammars/simp_expr_unambig4.smt2Pl-sentinel"
                   "grammars/simp_expr_unambig4.smt2Plus-sentinel"
                   "grammars/simp_expr_unambig4.smt2EX-sentinel"
                   "grammars/simp_expr_unambig4.smt2EY-sentinel"
                   "grammars/simp_expr_unambig4.smt2head-sentinel"
                   "grammars/simp_expr_unambig4.smt2tail-sentinel"
                   "grammars/simp_expr_unambig4.smt2Plus_0-sentinel"
                   "grammars/simp_expr_unambig4.smt2Plus_1-sentinel"
                   "grammars/simp_expr_unambig4.smt2append-sentinel"
                   "grammars/simp_expr_unambig4.smt2linTerm-sentinel"
                   "grammars/simp_expr_unambig4.smt2lin-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2nil-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2cons-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2sort2-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2insert2-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2zsplitAt-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2ztake-sentinel"
                   "tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2-sentinel"))

  (define qual (format-symbols (qual-all test-files)))

  (let* ([syms (format-symbols (symbols-of-theorems-s (read-benchmark qual)))])

    (for-each (lambda (sym)
                (with-check-info
                 (('sym     sym)
                  ('message "Native symbol was stripped"))
                 (check-false (member sym (string-split syms "\n")))))
              '("true-sentinel" "false-sentinel" "ite-sentinel" "or-sentinel"))

    (for-each (lambda (sym)
                (with-check-info
                 (('sym     sym)
                  ('syms    syms)
                  ('message "Found symbol"))
                 (check-not-equal? (member sym (string-split syms "\n"))
                                   #f)))
              subset)

      (let* ([syms (string-split (format-symbols (symbols-of-theorems-s test-defs))
                                 "\n")])
        (for-each (lambda (sym)
                    (with-check-info
                     (('sym       sym)
                      ('test-defs test-defs)
                      ('message   "Symbol is qualified"))
                     (check-true (string-contains? sym ".smt2")))

                    (with-check-info
                     (('sym       sym)
                      ('test-defs test-defs)
                      ('message   "Symbol has suffix"))
                     (check-true (string-contains? sym "-sentinel"))))
                  syms)))

  (test-case "No alpha-equivalent duplicates in result"
    (let* ([normalised (norm test-defs)])
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

  (check-equal? (get-def-s "constructorZ" redundancies)
                (list constructorZ))

  (for-each (lambda (sym)
    (define def (format-symbols (get-def sym qual)))

    (let ([count (length (filter non-empty-string?
                                 (string-split def "\n")))])
      (with-check-info
       (('sym     sym)
        ('def     def)
        ('count   count)
        ('message "Symbol got qualified"))
       (check-equal? count 1)))

    (define norm-def
      (format-symbols (get-def-s sym test-defs)))

    (define norm-count
      (length (filter non-empty-string?
                      (string-split norm-def "\n"))))

    (with-check-info
     (('sym      sym)
      ('norm-def norm-def)
      ('message  "Got single definition"))
     (check-true (< norm-count 2)))

    (when (equal? norm-count 1)
      ;; The symbols in norm-def may be replacements, so we can't compare
      ;; directly. Instead, we just infer the structure:
      (define (strip-non-paren s)
        (list->string (foldr (lambda (c str)
                               (if (or (equal? c #\()
                                       (equal? c #\)))
                                   (cons c str)
                                   str))
                             '()
                             (string->list s))))

      (define def-shape (strip-non-paren def))

      (define norm-shape (strip-non-paren norm-def))

      (with-check-info
       (('def        def)
        ('norm-def   norm-def)
        ('def-shape  def-shape)
        ('norm-shape norm-shape)
        ('message    "Duplicate removal kept definition intact"))
       (check-equal? def-shape norm-shape))))
    (take (shuffle subset) 5))

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
                       (foo.smt2baz-sentinel  ((x Nat)) Nat
                                              X)))
      (define-fun
        foo.smt2quux-sentinel ()        Bool
        (hello world))
      (define-fun-rec (par (a)
                           (bar.smt2quux-sentinel ()        Foo
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

  (check-equal? (map (curry map ~a)
                     (name-replacements-for qualified-example))
                '(("foo.smt2baz-sentinel" "baz")))

  (check-equal? (remove-prefices qualified-example)
                (cons '(define-fun (par (a b) (baz ((x Nat)) Nat X)))
                      (cdr qualified-example)))

  (check-equal? (prepare qualified-example)
                '((define-fun (par (a b)
                                   (baz  ((x Nat)) Nat
                                         X)))
                  (define-fun
                    foo.smt2quux ()        Bool
                    (hello world))
                  (define-fun-rec (par (a)
                                       (bar.smt2quux ()        Foo
                                                     (foo bar))))
                  (check-sat)))

  (check-equal? (list->set (find-redundancies redundancies))
                (list->set '((redundantZ1 constructorZ)
                             (redundantZ2 constructorZ)
                             (redundantZ3 constructorZ))))

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
                    (((defining-type-1
                        (normalise-constructor-2)
                        (normalise-constructor-1 (normalise-destructor-2 normalise-var-1)
                                                 (normalise-destructor-1 (defining-type-1 normalise-var-1))))))))

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
    (let* ([dir    (make-temporary-file "te-benchmark-temp-test-data-~a"
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

    (let ([data-count (length (filter (lambda (line)
                                        (string-prefix? line "data "))
                                      (string-split sig "\n")))])
      (with-check-info
       (('data-count data-count)
        ('sig        sig)
        ('message    "'Form' datatype appears in signature"))
       (check-equal? data-count 1))))

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

    (for-each (lambda (fun)
                (define def-count (length (filter (lambda (line)
                                                    (string-prefix? line fun))
                                                  (string-split sig "\n"))))

                (with-check-info
                 (('fun       fun)
                  ('def-count def-count)
                  ('sig       sig)
                  ('message   "Function defined in signature"))
                 (check-true (> def-count 0))))
              (list "models" "models2" "models5"))))

  (test-case "Single files"
    (define files (map (lambda (suf)
                         (string-append benchmark-dir "/" suf))
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

  (define test-benchmark-files
    (take (shuffle (theorem-files)) 10))

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
    (check-equal? names expect)))

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

    (let* ([f    (string-append benchmark-dir "/tip2015/int_right_distrib.smt2")]
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
                  "Nat")))
