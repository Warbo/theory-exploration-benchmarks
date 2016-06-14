(require racket/match)
(require racket/trace)

(define (read-benchmark x)
  (let* ([content (string-append "(\n" x "\n)")])
    (with-input-from-string content
      read)))

; Extract the symbols used by a benchmark expression
(define (expression-constructors exp)
  (match exp
         [(list 'declare-datatypes given decs) (constructors-from-def given decs)]
         [(cons a b)                           (append (expression-constructors a)
                                                       (expression-constructors b))]
         [_                                    null]))

(define (expression-destructors exp)
  (match exp
    [(list 'declare-datatypes given decs) (destructors-from-def decs)]
    [(cons a b)                           (append (expression-destructors a)
                                                  (expression-destructors b))]
    [_                                    null]))

(define (symbols-in exp)
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

(define (case-symbols c)
  (match c
         ;; Remove the symbols occuring in pat from body. This will remove fresh
         ;; variables, but may also remove constructors. That's fine though,
         ;; since we extract constructors separately anyway.
         [(list 'case pat body) (remove* (symbols-in pat) (symbols-in body))]
         [_                     (error "Unexpected case form")]))

(define (constructors-from-def given decs)
  (remove* (symbols-in given)
           (symbols-in (foldl (lambda (dec got)
                                (append got (match dec
                                              [(cons type defs) (symbols-in (map constructor-symbols defs))]
                                              [_                (error "Unexpected type definition")])))
                              null
                              decs))))

(define (destructors-from-def decs)
  (symbols-in (foldl (lambda (dec got)
                       (append got (match dec
                                     [(cons type defs) (symbols-in (map destructor-symbols defs))]
                                     [_                (error "Unexpected type def")])))
                     null
                     decs)))

(define (constructor-symbols c)
  (match c
         [(cons name vars) (list name)]
         [_                (error "Unexpected constructor form")]))

(define (destructor-symbols c)
  (match c
    [(cons name vars) (map car vars)]
    [_                (error "Unexpected destructor form")]))

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

;(trace fun-rec-expressions)

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
  (match defs
    [(cons h t) (append (con-types h) (constructor-types t))]
    [_          null]))

(define (con-types def)
  ;; Given (Cons (Cons_1 a) (Cons_2 (list a))) we want '(a list)
  (let* ([each (map (lambda (x) (symbols-in (cdr x))) (cdr def))]
         [out  (apply append each)])
    out))

(define (expression-symbols exp)
  (remove* (expression-types exp)
           (append (expression-constructors exp)
                   (expression-destructors  exp)
                   (expression-funs         exp))))

(define (dbg msg x)
  (eprintf (format "~a ~a\n" msg x))
  x)

(define (benchmark-types x)
  (remove-duplicates (symbols-in (expression-types (read-benchmark x)))))

(define (benchmark-symbols x)
  (remove-duplicates (expression-symbols (read-benchmark x))))

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
  (apply append (map (lambda (dec)
                       (cons (car dec) ; type name
                             (apply append (map (lambda (con)
                                                  (cons (car con) ; constructor name
                                                        (map car (cdr con)))) ; destructor names
                                                (cdr dec)))))
                     decs)))

(define (join-spaces xs)
  (if (empty? xs)
      ""
      (if (equal? (length xs) 1)
          (format "~a" (car xs))
          (format "~a ~a" (car xs) (join-spaces (cdr xs))))))
