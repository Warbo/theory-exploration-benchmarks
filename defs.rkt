(require racket/match)

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
  (if (symbol? exp)
      (list exp)
      (match exp
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
             [_                              null])))

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
    [(list (cons (list name args return) more-decs)
           (cons body                    more-defs)) (append (cons name (remove* (map car args)
                                                                                 (symbols-in body)))
                                                             (fun-rec-expressions more-decs more-defs))]
    [_                                                null]))

(define (expression-types exp)
  (dbg `(expression-types ,exp) (match exp
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
         [_                                            null])))

(define (constructor-types defs)
  (match defs
    [(cons h t) (append (con-types h) (constructor-types t))]
    [_          null]))

(define (con-types def)
  ;; Given (Cons (Cons_1 a) (Cons_2 (list a))) we want '(a list)
  (let* ([each (map (lambda (x) (symbols-in (cdr x))) (cdr def))]
         [out  (apply append each)])
    ;(eprintf (format "~a\n" `(GOT_CONSTRUCTOR_DEF ,def)))
    ;(eprintf (format "~a\n" `(EXTRACTED_TYPES     ,out)))
    out))

(define (expression-symbols exp)
  (remove* (expression-types exp)
           (append (expression-constructors exp)
                   (expression-destructors  exp)
                   (expression-funs         exp))))

(define (dbg msg x)
  ;(eprintf (format "~a ~a\n" msg x))
  x)

(define (benchmark-types x)
  (remove-duplicates (symbols-in (expression-types (read-benchmark x)))))

(define (benchmark-symbols x)
  (remove-duplicates (expression-symbols (read-benchmark x))))

(define (qualify name expr)
  (let* ([syms  (expression-symbols expr)]
         [types (expression-types   expr)]
         [all   (append syms types)])
    (qualify-all name (symbols-in all) expr)))

(define (qualify-all name all expr)
  (if (empty? all)
      expr
      (qualify-all name (cdr all) (replace-in (car all)
                                              (string-append name (symbol->string (car all)) "-sentinel")
                                              expr))))

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
                 (any (lambda (con-dec)
                        (equal? (symbol->string (car con-dec))
                                sym))
                      (cdr ty-dec)))
               ty-decs)))

(define (any f xs)
  (match xs
    [(cons a b) (or (f a) (any f b))]
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
