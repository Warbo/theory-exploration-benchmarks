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
             [(cons a b) (append (symbols-in a) (symbols-in b))]
             [_          null])))

(define (constructors-from-def given decs)
  (remove* given
           (foldl (lambda (dec got)
                    (append got (match dec
                                       [(cons type defs) (symbols-in defs)]
                                       [_                null])))
                  null
                  decs)))

(define (expression-funs exp)
  (match exp
         [(list 'define-fun     name args return body) (cons name (remove* (map car args) (symbols-in body)))]
         [(list 'define-fun-rec name args return body) (cons name (remove* (map car args) (symbols-in body)))]
         [(cons a b)                                   (append (expression-funs a)
                                                               (expression-funs b))]
         [_                                            null]))

(define (expression-types exp)
  (match exp
         [(list 'define-fun     name args return body) (cons return (map cadr args))]
         [(list 'define-fun-rec name args return body) (cons return (map cadr args))]
         [(list 'declare-datatypes given decs)         (map car decs)]
         [(cons a b)                                   (append (expression-types a)
                                                               (expression-types b))]
         [_                                            null]))

(define (expression-symbols exp)
  (remove* (append (list 'match 'case) (expression-types exp))
           (append (expression-constructors exp)
                   (expression-funs         exp))))

(define (benchmark-symbols x)
  (remove-duplicates (expression-symbols (read-benchmark x))))
