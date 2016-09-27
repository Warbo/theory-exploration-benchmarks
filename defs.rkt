#lang racket
(require racket/function)
(require racket/match)
(require racket/trace)

(provide qual-all)
(provide rec-names)
(provide prepare)
(provide mk-defs)
(provide all-names)
(provide replace-strings)
(provide find-redundancies)
(provide symbols-of-theorems)
(provide canonical-functions)
(provide get-con-def)
(provide qualify)
(provide theorems-from-symbols)

(module+ test
  (require rackunit))

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

#;(module+ test
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
  (remove-duplicates (expression-symbols (read-benchmark x))))

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

#;(module+ test
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
  (define nat-def      '(declare-datatypes () ((Nat (Z) (S (p Nat))))))
  (define constructorZ '(define-fun constructorZ ()              Nat Z))
  (define constructorS '(define-fun constructorS ((local-p Nat)) Nat (S local-p)))
  (check-equal? (add-constructor-funcs (list nat-def))
                `(,nat-def ,constructorZ ,constructorS)))

(define (qual-all)
  (define given-files
    (port->lines (current-input-port)))

  (define given-contents
    (map (lambda (pth)
           (list (string-replace
                  (string-join
                   (reverse (take (reverse (string-split pth "/")) 2))
                   "/") "'" "_tick_")
                 (read-benchmark (file->string pth))))
         given-files))

  (define qualified-contents
    (map (lambda (name-content)
           (qualify (first name-content) (second name-content)))
         given-contents))

  (define result
    (trim (write-s (apply append qualified-contents))))

  (displayln result))

(require shell/pipeline)

(define (mk-final-defs)
  (run-pipeline/out '(./mk_final_defs.sh)))

(define (mk-final-defs-2)
  (run-pipeline/out '(./mk_defs.rkt) '(./prepare.rkt)))

(define (mk-defs)
  (run-pipeline/out '(./qual_all.rkt) '(./norm_defs.sh)))

(define (mk-defs-s s)
  (pipe s (lambda () (run-pipeline '(./qual_all.rkt) '(./norm_defs.sh)))))

#;(module+ test
  (for-each (lambda (f)
              (check-equal? (string-trim (mk-defs-s f))
                            (string-trim (pipe f mk-defs))))
            '("modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig1.smt2"
              "modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig4.smt2"
              "modules/tip-benchmarks/benchmarks/tip2015/sort_StoogeSort2IsSort.smt2")))

(define (string->lines s)
  (string-split s "\n" #:trim? #f))

(define (trim s)
  (string-join (filter (lambda (x) (not (string-prefix? x "(assert-not ")))
                       (filter (lambda (x) (not (string-prefix? x "(check-sat)")))
                               (string->lines s)))
               "\n"))

#;(module+ test
  (check-equal? (trim "hello")                      "hello")
  (check-equal? (trim "foo\n(assert-not bar)\nbaz") "foo\nbaz")
  (check-equal? (trim "foo\n(check-sat)\nbar")      "foo\nbar"))

; Combine all definitions in files given on stdin

#;(module+ test
  (define f
    "modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig3.smt2")

  (define one-liners
    (run-pipeline/out `(echo ,f)
                      '(./qual_all.rkt)))

  (define result
    (map (lambda (line)
           (define names
             (run-pipeline/out `(echo ,line)
                               '(./rec_names.rkt)
                               '(tr "\n" " ")))
           (run-pipeline/out `(echo ,names)
                             '(grep "^.")
                             '(sed -e "s/[ ]*$//g")))
         (string-split one-liners "\n")))

  (define all-result
    (run-pipeline/out `(echo ,one-liners)
                      '(./all_names.rkt)
                      '(grep "^.")))
  (check-equal? (filter non-empty-string?
                        (string-split all-result "\n"))
                (filter non-empty-string?
                        (string-split (string-join result "\n") "\n"))))

; Check each function declaration syntax

(define (path p)
  ; Prefix our argument with the benchmarks directory, to save us typing it
  ; over and over
  (build-path (current-directory)
              "modules/tip-benchmarks/benchmarks"
              p))

(define (string->nonempties x)
  (filter (lambda (l) (not (equal? "" (string-trim l))))
          (string->lines x)))

(define (count-nonempties x)
  (length (string->nonempties x)))

(define (ss-eq? x y)
  (cond ([symbol? x]
         (ss-eq? (symbol->string x) y))
        ([symbol? y]
         (ss-eq?  x (symbol->string y)))
        (#t
         (equal? x y))))

#;(module+ test
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
    [(list 'define'fun (list 'par _ (list name _ _ _)))     (if (ss-eq? name f)
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

#;(module+ test
  (check-equal? (find-sub-exprs "constructorZ"
                                `(,nat-def ,constructorZ ,constructorS))
                (list constructorZ)))

(define (write-s x)
  (define o (open-output-string))
  (write x o)
  (get-output-string o))

(define (get-fun-def f)
  (define benchmarks
    (read-benchmark (port->string (current-input-port))))

  (write (string-join (map write-s (find-sub-exprs f benchmarks)) "\n")))

(define (as-str x)
  (if (string? x)
      x
      (symbol->string x)))

#;(module+ test
  (define (have-def defs name kind)
    (define def
      (pipe defs (lambda ()
                   (get-fun-def (string-append (as-str name) "-sentinel")))))

    (define count
      (count-nonempties def))

    (with-check-info
        (('defs    defs)
         ('def     def )
         ('kind    kind)
         ('message "Can get function definition"))
      (check-eq? count 1)))

  (test-case "Function declaration syntax"
    (let ((defs (run-pipeline/out
                 `(echo ,(path "tip2015/sort_StoogeSort2IsSort.smt2"))
                 '(./mk_defs.rkt))))
      (have-def defs "tip2015/sort_StoogeSort2IsSort.smt2sort2"        "plain")
      (have-def defs "tip2015/sort_StoogeSort2IsSort.smt2insert2"      "recursive")
      (have-def defs "tip2015/sort_StoogeSort2IsSort.smt2zsplitAt"     "parameterised")
      (have-def defs "tip2015/sort_StoogeSort2IsSort.smt2ztake"        "parameterised recursive")
      (have-def defs "tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2" "mutually recursive"))))

#;(module+ test
  (test-case "Real symbols qualified"
    (let* ([f "modules/tip-benchmarks/benchmarks/tip2015/propositional_AndCommutative.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_Sound.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_Okay.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/regexp_RecSeq.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/relaxedprefix_correct.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_AndIdempotent.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/propositional_AndImplication.smt2"]
           [q (run-pipeline/out `(echo ,f) '(./qual_all.rkt))]
           [s (run-pipeline/out `(echo ,q) '(./symbols_of_theorems.rkt))])

      (check-true (let ([result (run-pipeline/out `(echo ,s)
                                                  '(grep -F "or2-sentinel"))])
                    #t)
                  "Found an or2 symbol")

      (check-exn exn? (lambda ()
                        (run-pipeline/out `(echo ,s)
                                          '(grep -Fx "or2-sentinel")))
                 "or2 symbol is qualified")

      #;(let* ([d (pipe f mk-defs)]
             [s (pipe d (symbols-of-theorems))]
             [result (run-pipeline/out `(echo ,s) '(grep -F "or2-sentinel"))])
        (check-true (string? result) "Found 'or2' symbol"))))

  #;(let* ([files "modules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig1.smt2\nmodules/tip-benchmarks/benchmarks/grammars/simp_expr_unambig4.smt2\nmodules/tip-benchmarks/benchmarks/tip2015/sort_StoogeSort2IsSort.smt2"]
         [qual (run-pipeline/out `(echo ,files) '(./qual_all.rkt))]
         [syms (run-pipeline/out `(echo ,qual)  '(./symbols_of_theorems.rkt))])

    (test-case "Native symbols stripped"
      (for-each (lambda (sym)
                  (check-exn exn? (lambda ()
                                    (run-pipeline/out `(echo ,syms)
                                                      `(grep -Fx ,sym)))
                             (string-append "Native symbol " sym " was stripped")))
                '("true-sentinel" "false-sentinel" "ite-sentinel" "or-sentinel")))

    #;(test-case "Real symbols found"
               (for-each
                (lambda (sym)
                  (with-check-info
                   (('sym sym)
                    ('message (string-append "Found symbol for " sym)))
                   (check-true (string? (run-pipeline/out `(echo ,syms)
                                                          `(grep -Fx ,sym))))))
                '("grammars/simp_expr_unambig1.smt2append-sentinel"
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
                  "tip2015/sort_StoogeSort2IsSort.smt2stooge2sort2-sentinel")))

      #;(let* ([defs (run-pipeline/out `(echo ,files) '(./mk_defs.rkt))]
             [syms (string-split (run-pipeline/out `(echo defs)
                                                   '(./symbols_of_theorems.rkt)
                                                   '(grep "^."))
                                 "\n")])
        (for-each (lambda (sym)
                    (with-check-info
                     (('sym sym)
                      ('defs defs)
                      ('message (string-append "Symbol " sym " is qualified")))
                     (check-true (string? (run-pipeline/out `(echo ,sym)
                                                            '(grep "\\.smt2")))
                                 (string-append "Qualified symbol " sym)))

                    (with-check-info
                     (('sym  sym)
                      ('defs defs)
                      ('message (string-append "Symbol " sym " has suffix")))
                     (check-true (string? (run-pipeline/out `(echo ,sym)
                                                            '(grep -- "-sentinel$")))
                                 (string-append "Suffixed symbol " sym))))
                  syms))))

(define (rec-names)
  (define given-defs
    (map read-benchmark (port->lines (current-input-port))))

  (show (apply append (map names-in given-defs))))

#;(module+ test
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

(define (prepare)

  (define (addCheckSat x)
    ; Add '(check-sat)' as the last line to appease tip-tools
    (string-append x "\n(check-sat)"))

  (define (removeSuffices x)
    ; Removes '-sentinel' suffices. Do this after all other string-based
    ; transformations, since the sentinels prevent us messing with, say, the
    ; symbol "plus2", when we only wanted to change the symbol "plus"
    (string-replace x "-sentinel" ""))

  (define (removePrefices x)
    ; Removes unambiguous filename prefices
    (foldl (lambda (rep str)
             (define src (first  (string-split rep "\t")))
             (define dst (second (string-split rep "\t")))
             (string-replace str src dst))
           x
           (nameReplacements x)))
  (define (nameReplacements x)
    ; Unqualify any names which only have one definition
    ;
    ; For example, given:
    ;
    ; (define-fun foo.smt2baz-sentinel  ...)
    ; (define-fun foo.smt2quux-sentinel ...)
    ; (define-fun bar.smt2quux-sentinel ...)
    ;
    ; We can unqualify 'foo.smt2baz-sentinel' to get 'baz', but we can't for
    ; 'quux' since there are two distinct versions.
    (define nr-names (run-pipeline/out '(./rec_names.rkt)))

    (foldl (lambda (name rest)
             (if (string-contains? name ".smt2")
                 (let ([unqual (run-pipeline/out `(echo ,name)
                                                 '(sed -e "s/.*\\.smt2\\(.*\\)/\\1/g")
                                                 '(sed -e "s/-sentinel//g"))]
                       [count  (run-pipeline/out `(echo ,nr-names)
                                                 `(grep -cF (string-append ".smt2" unqual "-sentinel")))])
                   (if (equal? count "1")
                       (string-append name "\t" unqual "\n" rest)
                       rest))
                 rest))
           ""
           (string-split nr-names "\n")))

  ;(tag-types
    ;(tag-constructors
      ;(add-constructor-funcs
  (show
   (addCheckSat
    (removeSuffices
     (removePrefices
      (write-s
       (read-benchmark
        (port->string
         (current-input-port)))))))))

(define (all-names)
  (define given-defs
    (map read-benchmark (port->lines (current-input-port))))

  (define found-names
    (map (lambda (d)
           (join-spaces (names-in d)))
         given-defs))

  (show found-names)

  (exit)
  (show (apply append (map names-in given-defs))))

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
  (define redundancies `(,constructorZ
                         ,constructorS
                         (define-fun redundantZ1 () Nat Z)
                         (define-fun redundantZ2 () Nat Z)
                         (define-fun redundantZ3 () Nat Z)))
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

(define (symbols-of-theorems)
  (define (format-benchmark-symbols)
    (format-symbols (benchmark-symbols (port->string (current-input-port)))))

  (displayln (string-join (filter (lambda (s)
                                    (not (member s '("true-sentinel"
                                                     "false-sentinel"
                                                     "or-sentinel"
                                                     "ite-sentinel"))))
                                  (string-split (format-benchmark-symbols)
                                                "\n"))
                          "\n")))

(define (canonical-functions)
  (define given-functions
    (filter (lambda (x)
              (not (eof-object? x)))
            (map (lambda (line)
                   (with-input-from-string line
                     read))
                 (port->lines (current-input-port)))))

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

  (show (map norm given-functions)))

(define (pipe s f)
  (define o (open-output-string))
  (parameterize ([current-input-port  (open-input-string s)]
                 [current-output-port o])
    (f))
  (get-output-string o))

#;(module+ test
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

(define (strip-redundancies exprs reps)
  ; Remove alpha-equivalent expressions from exprs, according to reps
  (define replacements (map first reps))

  (foldl (lambda (line-defs result)
           (let ([keep      #t]
                 [line      (first  line-defs)]
                 [def-names (second line-defs)])
             (for-each (lambda (def-name)
                         (when (member def-name replacements)
                           (set! keep #f)))
                       def-names)
             (if keep
               (append result (list line))
               result)))
         '()
         (zip exprs (all-names-s exprs))))

(module+ test
  (check-equal? (list->set (strip-redundancies redundancies
                                               '((redundantZ1 constructorZ)
                                                 (redundantZ2 constructorZ)
                                                 (redundantZ3 constructorZ))))
                (list->set (list constructorZ constructorS))))
