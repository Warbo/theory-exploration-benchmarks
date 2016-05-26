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
  (remove* given
           (foldl (lambda (dec got)
                    (append got (match dec
                                       [(cons type defs) (symbols-in (map constructor-symbols defs))]
                                       [_                (error "Unexpected type definition")])))
                  null
                  decs)))

(define (constructor-symbols c)
  (match c
         [(cons name vars) (list name)]
         [_                (error "Unexpected constructor form")]))

(define (expression-funs exp)
  (match exp
         [(list 'define-fun-rec
                (list 'par p
                      (list name args return body)))   (cons name (remove* (map car args) (symbols-in body)))]
         [(list 'define-fun-rec name args return body) (cons name (remove* (map car args) (symbols-in body)))]
         [(list 'define-fun     name args return body) (cons name (remove* (map car args) (symbols-in body)))]

         [(cons a b)                                   (append (expression-funs a)
                                                               (expression-funs b))]
         [_                                            null]))

(define (expression-types exp)
  (match exp
         [(list 'define-fun-rec
                (list 'par p
                      (list name args return body)))   (cons return (map cadr args))]
         [(list 'define-fun-rec name args return body) (cons return (map cadr args))]
         [(list 'define-fun     name args return body) (cons return (map cadr args))]

         [(list 'declare-datatypes given decs)         (map car decs)]
         [(cons a b)                                   (append (expression-types a)
                                                               (expression-types b))]
         [_                                            null]))

(define (expression-symbols exp)
  (remove* (expression-types exp)
           (append (expression-constructors exp)
                   (expression-funs         exp))))

(define (benchmark-symbols x)
  (remove-duplicates (expression-symbols (read-benchmark x))))

(define (format-symbols syms)
  (if (null? syms)
      ""
      (format "~a\n~a" (car syms) (format-symbols (cdr syms)))))

(define (show x)
  (displayln (format-symbols x)))

(define (theorem-files)
  (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
          (sequence->list (in-directory "modules/tip-benchmarks/benchmarks"))))

(define (symbols-of-theorem path)
  (benchmark-symbols (file->string path)))
