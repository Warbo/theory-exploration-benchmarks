#lang racket
(require racket/function)
(require racket/match)
(require racket/trace)

(provide all-symbols)
(provide qual-all)
(provide rec-names)
(provide prepare)
(provide mk-defs)
(provide replace-strings)
(provide find-redundancies)
(provide symbols-of-theorems)
(provide canonical-functions)
(provide get-con-def)
(provide get-fun-def)
(provide qualify)
(provide qualify-given)
(provide theorems-from-symbols)
(provide strip-redundancies)
(provide mk-final-defs)
(provide mk-signature)

(module+ test
  (require rackunit)

  (define nat-def      '(declare-datatypes () ((Nat (Z) (S (p Nat))))))

  (define constructorZ '(define-fun constructorZ ()              Nat Z))

  (define constructorS '(define-fun constructorS ((local-p Nat)) Nat (S local-p)))

  (define redundancies `(,constructorZ
                         ,constructorS
                         (define-fun redundantZ1 () Nat Z)
                         (define-fun redundantZ2 () Nat Z)
                         (define-fun redundantZ3 () Nat Z))))

(define (symbols-in exp)
  (let ((case-symbols (lambda (c)
                        (match c
                          ;; Remove the symbols occuring in pat from body. This will remove fresh
                          ;; variables, but may also remove constructors. That's fine though,
                          ;; since we extract constructors separately anyway.
                          [(list 'case pat body) (remove* (symbols-in pat) (symbols-in body))]
                          [_                     (error "Unexpected case form")]))))
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
      [_                              (if (symbol? exp) (list exp) null)]))))

(module+ test
  (check-equal? (symbols-in '(lambda ((local1 Nat) (local2 (List Nat)))
                               (free1 local1)))
                '(free1)))

(define (expression-types exp)
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
                                                                        (symbols-in (map (lambda (x) (constructor-types (cdr x))) decs))))]
         [(cons a b)                                   (append (expression-types a)
                                                               (expression-types b))]
         [_                                            null]))

(define (constructor-types defs)
  (let ((con-types (lambda (def)
          ;; Given (Cons (Cons_1 a) (Cons_2 (list a))) we want '(a list)
          (concat-map (lambda (x) (symbols-in (cdr x)))
                      (cdr def)))))
    (match defs
      [(cons h t) (append (con-types h) (constructor-types t))]
      [_          null])))

; Extract the symbols used by a benchmark expression

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

(define (expression-funs exp)
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

(define (expression-symbols exp)
  (remove* (expression-types exp)
           (append (expression-constructors exp)
                   (expression-destructors  exp)
                   (expression-funs         exp))))

(define native-symbols
  (list 'Int 'Bool '* '> 'mod 'and 'or 'xor 'iff 'ite 'true 'false 'not 'implies
        'distinct '@ '= '<= '- '+ '* 'div '=> 'as))

(define (qualify name expr)
  (let* ([syms  (expression-symbols expr)]
         [types (expression-types   expr)]
         [all   (symbols-in (append syms types))])
    (qualify-all name all (prefix-locals expr))))

(define (qualify-all name all expr)
  (if (empty? all)
      expr
      (qualify-all name (cdr all) (replace-in (car all)
                                              (string-append name (symbol->string (car all)) "-sentinel")
                                              expr))))

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

(define (prefix-locals expr)
  ;; Turn bindings like (lambda (x Int) (f x)) into
  ;; (lambda (local-x Int) (f local-x)) to prevent conflicts between local and
  ;; global names (e.g. if there's a destructor called x)
  (prefix-all (locals-in expr) expr))

(define (prefix-all locals expr)
  (if (empty? locals)
      expr
      (prefix-all (cdr locals)
                  (replace-in (car locals)
                              (prefix-local (car locals))
                              expr))))

(define (prefix-local s)
  (string->symbol (string-append "local-" (symbol->string s))))

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

(define (zip xs ys)
  (if (empty? xs)
      null
      (if (empty? ys)
          null
          (cons (list (car xs) (car ys))
                (zip  (cdr xs) (cdr ys))))))

(define (theorem-files)
  (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
          (sequence->list (in-directory "modules/tip-benchmarks/benchmarks"))))

(define (types-of-theorem path)
  (benchmark-types (file->string path)))

(define (symbols-of-theorem path)
  (benchmark-symbols (file->string path)))

(define (defs-from sym exp)
  (match exp
    [(list 'declare-datatypes given decs) (find-defs sym given decs)]
    [(cons a b)                           (append (defs-from sym a)
                                                  (defs-from sym b))]
    [_                                    null]))

(define (find-defs sym given ty-decs)
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

(define (any-of f xs)
  (match xs
    [(cons a b) (or (f a) (any-of f b))]
    [_          #f]))

(define (files-with given)
  (filter (lambda (path)
            (member given (map symbol->string (symbols-of-theorem path))))
          (theorem-files)))

(define (read-benchmark x)
  (let* ([content (string-append "(\n" x "\n)")])
    (with-input-from-string content
      read)))

(define (benchmark-types x)
  (remove-duplicates (symbols-in (expression-types (read-benchmark x)))))

(define (benchmark-symbols x)
  (benchmark-symbols-expr (read-benchmark x)))

(define (benchmark-symbols-expr expr)
  (remove-duplicates (expression-symbols expr)))

(define (defs-of-src src given)
  (foldl (lambda (str rest)
           (append (defs-from given (read-benchmark str))
                   rest))
         '()
         src))

(define (defs-of given)
  (defs-of-src (map file->string (files-with given)) given))

(define (defs-of-stdin given)
  (defs-of-src (port->lines (current-input-port)) given))

(define (unique-defs-of given)
  (remove-duplicates (defs-of given)))

(define (collapse-defs syms)
  (foldl (lambda (sym rest)
           (append (unique-defs-of sym)
                   rest))
         '()
         syms))

(define (norm expr)
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
            [rec        (norm-case pat norm-body)]
            [norm-pat   (first rec)]
            [norm-body2 (second rec)])
       (list 'case norm-pat norm-body2))]

    [(list 'lambda args body)
     (let ([rec (norm-func args body)])
       (list 'lambda (first rec) (second rec)))]

    [(list 'let bindings body)
     (let* ([rec (norm-let bindings (norm body))])
       (list 'let (first rec) (second rec)))]

    [(cons a b) (cons (norm a) (norm b))]

    [_ expr]))

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

(define (norm-case pat body)
  (match pat
    [(list con)    (list pat body)]
    [(cons con ps) (let* ([rec       (norm-case2 ps body)]
                          [norm-ps   (first rec)]
                          [norm-body (second rec)])
                     (list (cons con norm-ps) norm-body))]
    [_             (list pat body)]))

(define (norm-case2 ps body)
  (if (empty? ps)
      (list ps body)
      (let* ([p         (car ps)]
             [rec       (norm-case2 (cdr ps) body)]
             [norm-ps   (first rec)]
             [norm-body (second rec)]
             [name      (next-var (list norm-ps norm-body))])
        (list (cons name norm-ps)
              (replace-in p name norm-body)))))

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

(define (norm-types decs)
  (if (empty? decs)
      decs
      (let ([rec (norm-types (cdr decs))])
        (cons (norm-type (car decs) rec) rec))))

(define (norm-type-params ps decs)
  (if (empty? ps)
      (list ps decs)
      (let* ([rec  (norm-type-params (cdr ps) decs)]
             [name (inc-name var-prefix (max-name var-prefix rec))])
        (list (cons name (first rec))
              (replace-in (car ps)
                          name
                          (second rec))))))

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

(define (norm-type dec rest)
  (let ([name (inc-name type-prefix (max-name type-prefix rest))]
        [cs   (norm-constructors (cdr dec) rest)])
    (cons name (replace-in (car dec) name cs))))

(define (inc-name pre n)
  (string->symbol (string-append pre (number->string (+ 1 n)))))

(define (norm-constructors cs rest)
  (if (empty? cs)
      cs
      (let ([rec (norm-constructors (cdr cs) rest)])
        (cons (norm-constructor (car cs) (list rest rec)) rec))))

(define (norm-constructor c rest)
  (let ([name (inc-name constructor-prefix (max-name constructor-prefix rest))])
    (cons name (norm-destructors (cdr c) rest))))

(define (norm-destructors ds rest)
  (if (empty? ds)
      ds
      (let* ([rec  (norm-destructors (cdr ds) rest)]
             [name (inc-name destructor-prefix (max-name destructor-prefix (list rest rec)))])
        (cons (cons name (cdr (car ds))) rec))))

(define        type-prefix "defining-type-")
(define constructor-prefix "normalise-constructor-")
(define  destructor-prefix "normalise-destructor-")
(define         var-prefix "normalise-var-")

(define (max-name pre expr)
  (match expr
    [(cons a b) (let ([ma (max-name pre a)]
                      [mb (max-name pre b)])
                  (if (< ma mb) mb ma))]
    [x          (name-num pre x)]))

(define (name-num pre x)
  (if (symbol? x)
      (if (string-prefix? (symbol->string x) pre)
          (string->number (substring (symbol->string x)
                                     (string-length pre)))
          0)
      0))

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

(define (join-spaces xs)
  (if (empty? xs)
      ""
      (if (equal? (length xs) 1)
          (format "~a" (car xs))
          (format "~a ~a" (car xs) (join-spaces (cdr xs))))))

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

(define (arg-decs-for c x)
  ;; Look through x for any definitions of c, and return its argument list
  (let ([arg-decs-for-ty (lambda (c x)
          (let ((arg-decs-for-con (lambda (c x)
                  (match x
                    [(list name) (if (equal? name c)
                                     (list '())
                                     '())]
                    [(cons name args) (if (equal? name c)
                                          (list args)
                                          '())]))))
            (concat-map (curry arg-decs-for-con c) (cdr x))))])
    (match x
      [(list 'declare-datatypes _ decs) (concat-map (curry arg-decs-for-ty c) decs)]
      [(cons h t) (append (arg-decs-for c h)
                          (arg-decs-for c t))]
      [_ '()])))

(define (constructor-type c x)
  (let* ((constructor-type-ty (lambda (c dec)
           (concat-map (lambda (con)
                         (if (equal? (car con) c)
                             (list (car dec))
                             '()))
                       (cdr dec)))))
    (match x
      [(list 'declare-datatypes _ decs) (concat-map (curry constructor-type-ty c) decs)]
      [(cons a b) (append (constructor-type c a) (constructor-type c b))]
      [_ '()])))

(define (concat-map f xs)
  (apply append (map f xs)))

(module+ test
  (check-equal? (concat-map (lambda (x) (list x x x))
                            '(fee fi fo fum))
                '(fee fee fee fi fi fi fo fo fo fum fum fum)))

(define (func-for x c)
  (let ([arg-apps-for (lambda (c x)
           (map car (car (arg-decs-for c x))))]

        [arg-decs (car (arg-decs-for c x))])
    (prefix-locals
     `(define-fun
        ,(prefix-name c "constructor")
        ,arg-decs
        ,(car (constructor-type c x))
        ,(if (empty? arg-decs)
             c
             (cons c (arg-apps-for c x)))))))

(define (add-constructor-funcs x)
  ;; Adding function for each constructor
  (let* ([consts (expression-constructors x)])
    (append x (map (curry func-for x) consts))))

(module+ test
  (check-equal? (add-constructor-funcs (list nat-def))
                `(,nat-def ,constructorZ ,constructorS)))

(define (take-from-end n lst)
  (reverse (take (reverse lst) n)))

(define (trim lst)
  (filter (lambda (x)
            (and (not (equal? (first x) 'assert-not))
                 (not (equal? x '(check-sat)))))
          lst))

(define (qual-all)
  (show (qual-all-s (port->lines (current-input-port)))))

(define (qual-all-s given-files)
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

(require shell/pipeline)

(define (mk-defs)
  (show (mk-defs-s (port->lines (current-input-port)))))

(define (mk-defs-s given-files)
  (norm-defs-s (qual-all-s given-files)))

(module+ test
  (for-each (lambda (f)
              (check-equal? (map ~a (mk-defs-s (string-split f "\n")))
                            (string-split (string-trim (pipe f mk-defs)) "\n")))
            '("modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig1.smt2"
              "modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig4.smt2"
              "modules/tip-benchmarks/benchmarks/tip2015/sort_StoogeSort2IsSort.smt2")))

(define (string->lines s)
  (string-split s "\n" #:trim? #f))

(module+ test
  (check-equal? (trim '((hello)))                      '((hello)))
  (check-equal? (trim '((foo) (assert-not bar) (baz))) '((foo) (baz)))
  (check-equal? (trim '((foo) (check-sat) (bar)))      '((foo) (bar))))

; Combine all definitions in files given on stdin

(define (non-empty? x)
  (not (empty? x)))

(module+ test
  (define f
    "modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig3.smt2")

  (define one-liners
    (qual-all-s (string-split f "\n")))

  (define result
    (filter non-empty?
            (map (lambda (expr)
                   (filter non-empty? (rec-names-s (list expr))))
                 one-liners)))

  (define all-result
    (filter non-empty? (all-names-s one-liners)))

  (with-check-info
   (('f          f)
    ('one-liners one-liners)
    ('result     result)
    ('all-result all-result))
   (check-equal? all-result result)))

; Check each function declaration syntax

(define (path p)
  ; Prefix our argument with the benchmarks directory, to save us typing it
  ; over and over
  (path->string (build-path (current-directory)
                            "modules/tip-benchmarks/benchmarks"
                            p)))

(define (ss-eq? x y)
  (cond ([symbol? x]
         (ss-eq? (symbol->string x) y))
        ([symbol? y]
         (ss-eq?  x (symbol->string y)))
        (#t
         (equal? x y))))

(module+ test
  (check-true  (ss-eq? 'foo  'foo))
  (check-true  (ss-eq? 'foo  "foo"))
  (check-true  (ss-eq? "foo" 'foo))
  (check-true  (ss-eq? "foo" "foo"))

  (check-false (ss-eq? 'foo  'bar))
  (check-false (ss-eq? 'foo  "bar"))
  (check-false (ss-eq? "foo" 'bar))
  (check-false (ss-eq? "foo" "bar")))

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

(module+ test
  (check-equal? (find-sub-exprs "constructorZ"
                                `(,nat-def ,constructorZ ,constructorS))
                (list constructorZ))

  (let* ([file "tip2015/sort_StoogeSort2IsSort.smt2"]
         [defs (mk-defs-s (list (path file)))])
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
                ("stooge2sort2" "mutually recursive")))))

(define (get-fun-def f)
  (show (find-sub-exprs f (read-benchmark (port->string (current-input-port))))))

(define (as-str x)
  (if (string? x)
      x
      (symbol->string x)))

(module+ test
  (define test-files
    (string-join (map (curry string-append
                             "modules/tip-benchmarks/benchmarks/")
                      '("grammars/simp_expr_unambig1.smt2"
                        "grammars/simp_expr_unambig4.smt2"
                        "tip2015/sort_StoogeSort2IsSort.smt2"))
                 "\n"))

  (define test-defs
    (pipe test-files mk-defs))

  (test-case "Real symbols qualified"
    (let* ([f "modules/tip-benchmarks/benchmarks/tip2015/propositional_AndCommutative.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_Sound.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_Okay.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/regexp_RecSeq.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/relaxedprefix_correct.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_AndIdempotent.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_AndImplication.smt2"]
           [q (pipe f qual-all)]
           [s (pipe q symbols-of-theorems)])

      (check-true (string-contains? s "or2-sentinel")
                  "Found an or2 symbol")

      (check-false (member "or2-sentinel" (string-split s "\n"))
                   "or2 symbol is qualified")

      (let* ([d (pipe f mk-defs)]
             [s (pipe d symbols-of-theorems)])
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

  (define qual (pipe test-files qual-all))

  (let* ([syms (pipe qual symbols-of-theorems)])

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

      (let* ([syms (string-split (pipe test-defs symbols-of-theorems)
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
    (let* ([normalised (pipe test-defs canonical-functions)])
      (for-each (lambda (norm)
                  (define norms
                    (filter (curry equal? norm)
                            (read-benchmark normalised)))
                  (unless (equal? norm '(check-sat))
                    (with-check-info
                     (('norms      norms)
                      ('norm       norm)
                      ('normalised normalised)
                      ('message    "Duplicate normalised forms!"))
                     (check-equal? norms (list norm)))))
                (read-benchmark normalised))))

  (for-each (lambda (sym)
    (define def (run-pipeline/out `(echo ,qual)
                                  `(./get_def.sh ,sym)))
    (define count
      (length (filter non-empty-string?
                      (string-split def "\n"))))

    (with-check-info
     (('sym     sym)
      ('def     def)
      ('message "Symbol got qualified"))
     (check-equal? count 1))

    (define norm-def (run-pipeline/out `(echo ,test-defs)
                                       `(./get_def.sh ,sym)))
    (define norm-count
      (length (filter non-empty-string?
                      (string-split norm-def "\n"))))

    (with-check-info
     (('sym      sym)
      ('norm-def norm-def)
      ('message  "Got single definition"))
     (check-true (< norm-count 2)))

    (when (equal? norm-count 1)
      ; The symbols in norm-def may be replacements, so we can't compare
      ; directly. Instead, we just infer the structure:
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
    (take (shuffle subset) 5)))

(define (rec-names)
  (show (rec-names-s (read-benchmark (port->string (current-input-port))))))

(define (rec-names-s exprs)
  (apply append (map names-in exprs)))

(module+ test
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
                '(stooge1sort2 stoogesort stooge1sort1)))

(define (add-check-sat x)
  ; Add '(check-sat)' as the last line to appease tip-tools
  (append x '((check-sat))))

(define (remove-suffices x)
  ; Removes '-sentinel' suffices. Do this after all other string-based
  ; transformations, since the sentinels prevent us messing with, say, the
  ; symbol "plus2", when we only wanted to change the symbol "plus"
  (read-benchmark (string-replace (format-symbols x)
                                  "-sentinel"
                                  "")))

(module+ test
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
                      (foo bar)))))))

(define (name-replacements-for x)
  ; Unqualify any names which only have one definition
  (define nr-names (rec-names-s x))

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

(module+ test
  (check-equal? (map (curry map ~a)
                     (name-replacements-for qualified-example))
                '(("foo.smt2baz-sentinel" "baz"))))

(define (remove-prefices x)
  ; Removes unambiguous filename prefices
  (read-benchmark (foldl (lambda (rep str)
                           (string-replace str (first rep) (second rep)))
                         (format-symbols x)
                         (name-replacements-for x))))

(module+ test
  (check-equal? (remove-prefices qualified-example)
                (cons '(define-fun (par (a b) (baz ((x Nat)) Nat X)))
                      (cdr qualified-example))))

(define (prepare-s x)
  ;(tag-types
    ;(tag-constructors
      ;(add-constructor-funcs
  (add-check-sat (remove-suffices (remove-prefices x))))

(define (prepare)
  (show (prepare-s (read-benchmark (port->string (current-input-port))))))

(module+ test
  (check-equal? (prepare-s qualified-example)
                '((define-fun (par (a b)
                    (baz  ((x Nat)) Nat
                      X)))
                  (define-fun
                     foo.smt2quux ()        Bool
                      (hello world))
                  (define-fun-rec (par (a)
                    (bar.smt2quux ()        Foo
                      (foo bar))))
                  (check-sat))))

(define (all-names)
  (show (map (lambda (line)
               (join-spaces (names-in (read-benchmark line))))
             (port->lines (current-input-port)))))

(define (all-names-s exprs)
  (map names-in exprs))

(define (find-redundancies)
  (show (map (lambda (x)
               (format "~a\t~a" (first x) (second x)))
             (find-redundancies-s (map read-benchmark
                                       (filter non-empty-string?
                                               (port->lines (current-input-port))))))))

(define (find-redundancies-s exprs)
  (define (mk-output expr so-far name-replacements)
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

(module+ test
  (check-equal? (list->set (string-split (string-trim (pipe (format-symbols redundancies)
                                                            find-redundancies))
                                         "\n"))
                (list->set '("redundantZ1\tconstructorZ"
                             "redundantZ2\tconstructorZ"
                             "redundantZ3\tconstructorZ")))

  (check-equal? (list->set (find-redundancies-s redundancies))
                (list->set '((redundantZ1 constructorZ)
                             (redundantZ2 constructorZ)
                             (redundantZ3 constructorZ)))))

(define (symbols-of-theorems-s expr)
  (filter (lambda (s)
            (not (member s '(true-sentinel
                             false-sentinel
                             or-sentinel
                             ite-sentinel))))
          (benchmark-symbols-expr expr)))

(define (symbols-of-theorems)
  (displayln (string-join (map ~a (symbols-of-theorems-s
                                   (read-benchmark
                                    (port->string (current-input-port)))))
                          "\n")))

(define (canonical-functions)
  (show (canonical-functions-s (port->lines (current-input-port)))))

(define (canonical-functions-s x)
  (define (normalise def)
    (match def
      [(list 'define-fun
             (list 'par p
                   (list name args return body))) (list 'define-fun
                                                        (list 'par ))]
      [(list name args typ body) (mk-def name args typ (normalise-cases body))]
      [_                         (error "Function definition of unexpected form" def)]))

  (define (normalise-cases exp)
    (match exp
      [(list 'case pat body) (list 'case
                                   (replace-pat pat body)
                                   (replace-case pat (normalise-cases body)))]
      [(cons a b)            (cons (normalise-cases a) (normalise-cases b))]
      [_                     exp]))

  (define (replace-pat pat body)
    (match pat
      [(cons con args) (cons con
                             (get-normal-pat args (+ 1 (highest-var body))))]
      [con             con]))

  (define (highest-var exp)
    (if (string? exp)
        (if (string-prefix? exp "var")
            (string->number (substring exp 3))
            -1)
        (match exp
          [(cons a b) (max (highest-var a) (highest-var b))]
          [_          -1])))

  (define (replace-case pat body)
    (match pat
      [(list con)      body]
      [(cons con args) (replace-args args (+ 1 (highest-var body)) body)]
      [con             body]
      [_               (error "Unexpected case structure" pat)]))

  (define (mk-def name args typ body)
    (list name
          (get-normal-args args 0)
          typ
          (replace-args (map car args) 0 body)))

  (define (nth-arg n)
    (string-append "var" (number->string n)))

  (define (replace-args args n body)
    (match args
      [(cons arg rest) (replace-args rest
                                     (+ 1 n)
                                     (replace-in-canon arg (nth-arg n) body))]
      [null            body]))

  (define (replace-in-canon src dst exp)
    (if (equal? src exp)
        dst
        (match exp
          [(cons a b) (cons (replace-in-canon src dst a)
                            (replace-in-canon src dst b))]
          [_          exp])))

  (define (get-normal-pat args n)
    (match args
      [(cons name rest) (cons (nth-arg n)
                              (get-normal-pat rest (+ 1 n)))]
      [_                null]))

  (define (get-normal-args args n)
    (match args
      [(cons (list name typ) rest) (cons (list (nth-arg n) typ)
                                         (get-normal-args rest (+ 1 n)))]
      [_                           null]))

  (map norm (filter (lambda (x)
                      (not (eof-object? x)))
                    (map (lambda (line)
                           (with-input-from-string line
                             read))
                         x))))

(define (pipe s f)
  (define o (open-output-string))
  (parameterize ([current-input-port  (open-input-string s)]
                 [current-output-port o])
    (f))
  (get-output-string o))

(module+ test
  (define (checkNormal kind def expected)
    (define canon (pipe def canonical-functions))

    (with-check-info
        (('kind     kind)
         ('def      def)
         ('expected expected)
         ('message  "Normalising as expected"))
      (check-equal? (string-trim canon) expected)))

  (checkNormal "function"
               "(define-fun sort2 ((x Int) (y Int)) (list Int) (ite (<= x y) (cons x (cons y (as nil (list Int)))) (cons y (cons x (as nil (list Int))))))"
               "(define-fun defining-function-1 ((normalise-var-2 Int) (normalise-var-1 Int)) (list Int) (ite (<= normalise-var-2 normalise-var-1) (cons normalise-var-2 (cons normalise-var-1 (as nil (list Int)))) (cons normalise-var-1 (cons normalise-var-2 (as nil (list Int))))))")

  (checkNormal "parameterised function"
               "(define-fun (par (a) (zsplitAt ((x Int) (y (list a))) (Pair (list a) (list a)) (Pair2 (ztake x y) (zdrop x y)))))"
               "(define-fun (par (normalise-var-3) (defining-function-1 ((normalise-var-2 Int) (normalise-var-1 (list normalise-var-3))) (Pair (list normalise-var-3) (list normalise-var-3)) (Pair2 (ztake normalise-var-2 normalise-var-1) (zdrop normalise-var-2 normalise-var-1)))))")

  (checkNormal "datatype"
               "(declare-datatypes (a) ((list (nil) (cons (head a) (tail (list a))))))"
               "(declare-datatypes (normalise-var-1) (((defining-type-1 (normalise-constructor-2) (normalise-constructor-1 (normalise-destructor-2 normalise-var-1) (normalise-destructor-1 (defining-type-1 normalise-var-1)))))))")

  (checkNormal "let binding"
               "(define-fun-rec msorttd ((x (list Int))) (list Int) (let ((k (div (zlength x) 2))) (lmerge (msorttd (ztake k x)) (msorttd (zdrop k x)))))"
               "(define-fun-rec defining-function-1 ((normalise-var-2 (list Int))) (list Int) (let ((normalise-var-1 (div (zlength normalise-var-2) 2))) (lmerge (defining-function-1 (ztake normalise-var-1 normalise-var-2)) (defining-function-1 (zdrop normalise-var-1 normalise-var-2)))))")

  (checkNormal "pattern match"
               "(define-fun-rec s ((x Bin)) Bin (match x (case One (ZeroAnd One)) (case (ZeroAnd xs) (OneAnd xs)) (case (OneAnd ys) (ZeroAnd (s ys)))))"
               "(define-fun-rec defining-function-1 ((normalise-var-2 Bin)) Bin (match normalise-var-2 (case One (ZeroAnd One)) (case (ZeroAnd normalise-var-1) (OneAnd normalise-var-1)) (case (OneAnd normalise-var-1) (ZeroAnd (defining-function-1 normalise-var-1)))))")

  (checkNormal "anonymous function"
               "(define-fun-rec qsort ((y Int) (xs (list Int))) (list Int) (append (append (qsort (filter (lambda ((z Int)) (<= z y)) xs)) (cons y (as nil (list Int)))) (qsort (filter (lambda ((x2 Int)) (> x2 y)) xs))))"
               "(define-fun-rec defining-function-1 ((normalise-var-3 Int) (normalise-var-2 (list Int))) (list Int) (append (append (defining-function-1 (filter (lambda ((normalise-var-1 Int)) (<= normalise-var-1 normalise-var-3)) normalise-var-2)) (cons normalise-var-3 (as nil (list Int)))) (defining-function-1 (filter (lambda ((normalise-var-1 Int)) (> normalise-var-1 normalise-var-3)) normalise-var-2))))"))

(define (get-con-def)

  (define name
    (getenv "NAME"))

  (show (remove-duplicates (defs-of-stdin name))))

(define (qualify-given)
  (define given
    (read-benchmark (port->string (current-input-port))))

  (define name
    (string-replace (getenv "NAME") "'" "_tick_"))

  (show (qualify name given)))

(define (theorems-from-symbols)
  (define given-symbols
    (port->lines (current-input-port)))

  (define (acceptable-theorem thm-path)
    (null? (remove* (cons "" given-symbols)
                    (map symbol->string (symbols-of-theorem thm-path)))))

  (define (acceptable-theorems)
    (filter acceptable-theorem (theorem-files)))

  (displayln (format-symbols (acceptable-theorems))))

(define (replace-strings file)
  (replace-strings-s (port->string (current-input-port))
                     (map (lambda (line)
                            (string-split line "\t"))
                          (filter non-empty-string?
                                  (file->lines file)))))

(define (replace-strings-s str reps)
  ; For each (src dst) in reps, replaces src with dst in str
  (foldl (lambda (pair so-far)
             (string-replace so-far (first pair) (second pair)))
         str
         reps))

(module+ test
  (check-equal? (replace-strings-s "hello mellow yellow fellow"
                                   '(("lo" "LO") ("el" "{{el}}")))
                "h{{el}}LO m{{el}}LOw y{{el}}LOw f{{el}}LOw")

  (let* ([formatted (format-symbols redundancies)]
         [reps      (pipe formatted find-redundancies)]
         [replaced  (replace-strings-s formatted (map (lambda (line)
                                                        (string-split line "\t"))
                                                      (filter non-empty-string?
                                                              (string-split reps "\n"))))]
         [defs      (read-benchmark replaced)])
    (check-equal? (list->set defs)
                  (list->set `(,constructorZ ,constructorS)))))

(define (strip-redundancies-s exprs)
  ; Remove alpha-equivalent expressions from exprs, according to reps
  (define redundancies (find-redundancies-s exprs))
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

  (read-benchmark (replace-strings-s (format-symbols stripped)
                                     (map (curry map ~a) redundancies))))

(define (strip-redundancies)
  (show (strip-redundancies-s (read-benchmark (port->string (current-input-port))))))

(module+ test
  (check-equal? (list->set (strip-redundancies-s redundancies))
                (list->set (list constructorZ constructorS)))

  (check-equal? (list->set (read-benchmark (pipe (format-symbols redundancies)
                                                 strip-redundancies)))
                (list->set (list constructorZ constructorS))))

(define (norm-defs)
  (show (norm-defs-s (read-benchmark (port->string (current-input-port))))))

(define (norm-defs-s exprs)
  (let ([norm (strip-redundancies-s exprs)])
    (if (equal? exprs norm)
        norm
        (norm-defs-s norm))))

(module+ test
  (let* ([given '((define-fun min1 ((x Int) (y Int)) Int (ite (<= x y) x y))
                  (define-fun min2 ((a Int) (b Int)) Int (ite (<= a b) a b)))]
         [defs  (norm-defs-s given)]
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
         [defs  (norm-defs-s given)]
         [syms  (symbols-of-theorems-s
                 (filter (lambda (expr)
                           (member 'fun3 expr))
                         defs))])
    (with-check-info
     (('defs    defs)
      ('syms    syms)
      ('message "References to discarded duplicates are replaced"))
     (check-not-equal? (member 'min1 syms) #f))))

(define (all-symbols)
  (show (sort (filter non-empty?
                      (foldl (lambda (path rest)
                               (append (symbols-of-theorem path) rest))
                             '()
                             (theorem-files)))
              (lambda (x y) (string<? (~a x) (~a y))))))

(define (defs-to-sig x)
  (pipe (pipe x mk-final-defs) mk-signature))

(module+ test
  (define test-dir "modules/tip-benchmarks/benchmarks")

  (define (in-temp-dir f)
    (let* ([dir    (make-temporary-file "te-benchmark-temp-test-data-~a"
                                        'directory)]
           [result (f dir)])
      (delete-directory/files dir)
      result))

  (define (string-to-haskell val)
    ; mk-final-defs takes in filenames, so it can qualify names. This makes
    ; and cleans up temporary files for testing.

    ; Note: We make a file in a directory, to avoid problems if tmpdir begins
    ; with a number (e.g. '/var/run/user/1000'); otherwise qualified variable
    ; names would be invalid
    (in-temp-dir (lambda (dir)
                   (define temp-file (string-append (path->string dir)
                                                    "/test.smt2"))

                   (display-to-file val temp-file)

                   (let* ([result (defs-to-sig temp-file)])
                     (delete-file temp-file)
                     result))))

  (define (count-substrings str sub)
    (- (length (string-split str sub)) 1))

  (test-case "Form"
    (define form
      '(declare-datatypes ()
         ((Form (& (&_0 Form) (&_1 Form))
                (Not (Not_0 Form))
                (Var (Var_0 Int))))))

    (define prepared
      (prepare-s form))

    (with-check-info
     (('prepared prepared)
      ('message  "Prepared 'Form' input defines a datatype"))
     (check-not-equal? #f
                       (member 'declare-datatypes (flatten prepared))))

    (define sig (string-to-haskell form))

    (define data-count (length (filter (lambda (line)
                                         (string-prefix? line "data "))
                                       (string-split sig "\n"))))

    (with-check-info
     (('data-count data-count)
      ('sig        sig)
      ('message    "'Form' datatype appears in signature"))
     (check-equal? data-count 1)))

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

    (define prepared (prepare-s mut))

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
                         (string-append test-dir "/" suf))
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
      (string-join (map (curry string-append test-dir "/")
                        '("tip2015/tree_SwapAB.smt2"
                          "tip2015/list_z_count_nub.smt2"))
                   "\n"))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('message "Local variables renamed")
      ('sig     sig))
     (check-true (string-contains? sig "local"))))

  (define benchmark-files
    (string-split (run-pipeline/out
                   '(find modules/tip-benchmarks/benchmarks/ -name "*.smt2")
                   '(shuf))
                  "\n"))

  (test-case "Random files"
    (define (files n)
      (string-join (take benchmark-files n) "\n"))

    (for-each (lambda (n)
                (define sig
                  (defs-to-sig (files n)))

                (with-check-info
                 (('n       n)
                  ('files   files)
                  ('sig     sig)
                  ('message "Made Haskell for random files"))
                 (check-true (string-contains? sig "QuickSpec"))))
              '(1 2 4 8))))

(define (mk-final-defs)
  (let ([input (port->string (current-input-port))])
    (display (pipe (pipe input mk-defs) prepare))))

(define (with-temp-file data proc)
  (let* ([f      (make-temporary-file "te-benchmark-temp-~a")]
         [result void])
    (display-to-file data f #:exists 'replace)
    (set! result (proc f))
    (delete-file f)
    result))

(module+ test
  (check-equal? (with-temp-file "foo\nbar\nbaz"
                                file->string)
                "foo\nbar\nbaz"))

(define (dump x) (write x (current-error-port)))

(define (mk-signature)
  (define input (port->string (current-input-port)))

  (display
   (with-temp-file input (lambda (f)
                           (run-pipeline/out `(tip ,f --haskell-spec))))))

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

(module+ test
  (let ([a (getenv "HOME")]
        [b (parameterize-env '([#"HOME" #"foo"])
                             (lambda () (getenv "HOME")))]
        [c (getenv "HOME")])
    (check-equal? a c)
    (check-equal? b "foo")))

(module+ test
  (test-case "Module tests"
    (for-each (lambda (n)
      (define files
        (string-join (take benchmark-files n) "\n"))

      (in-temp-dir (lambda (dir)
        (define out-dir (path->string dir))
        (parameterize-env `([#"FILES"   ,(string->bytes/utf-8 files)]
                            [#"OUT_DIR" ,(string->bytes/utf-8 out-dir)])
          (lambda ()
            (define these
              (string-join (take (string-split files "\n")
                                 n)
                           "\n"))

            (run-pipeline/out `(echo ,(pipe these mk-final-defs))
                              '(./full_haskell_package.sh))

            (with-check-info
             (('n       n)
              ('these   these)
              ('files   files)
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

              ; If the import fails, we're stuck with the Prelude, which contains classes
              (with-check-info
               (('out     out)
                ('message "Module imported successfully"))
               (check-false (string-contains? out "class Functor")))))))))
      '(1 3 5))))

(module+ test
  (for-each (lambda (t)
              (test-case (string-append "Test script " t)
                (in-temp-dir (lambda (d)
                  (parameterize-env `([#"HOME" ,(string->bytes/utf-8 (path->string d))])
                    (lambda ()
                      (let ([result (run-pipeline/out
                                     `(,(string->symbol
                                         (string-append "./" t))))])
                        (check-true  (string-contains? result "ok -"))
                        (check-false (string-contains? result "not ok")))))))))
            (map path->string (sequence->list (in-directory "tests")))))
