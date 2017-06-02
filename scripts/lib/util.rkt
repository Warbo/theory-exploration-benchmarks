#lang racket

(provide as-str
         decode16 define/test-contract
         deterministic-shuffle encode16 format-symbols hash-foldl index-where
         map-set non-empty? prefix-name
         read-benchmark replace-all replace-strings
         replace-in show string-reverse)

(module+ test
  (require rackunit))

;; Uses 'define/contract' when specified by the environment (e.g. during
;; testing), and 'define' otherwise. Useful since 'define/contract' can be very
;; slow, e.g. checking every recursive call.
(define-syntax (define/test-contract stx)
  (syntax-case stx ()
    [(define/test-contract sig contract body ...)
     (if (and (getenv "PLT_TR_CONTRACTS") #t)
         #'(define/contract sig contract body ...)
         #'(define          sig          body ...))]))

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
  (foldl (lambda (rep expr)
           (replace-in (first rep) (second rep) expr))
         expr
         reps))

;; For each (SRC DST) in REPS, replaces SRC with DST in STR
(define (replace-strings str reps)
  (foldl (lambda (pair so-far)
           (string-replace so-far (as-str (first  pair))
                           (as-str (second pair))))
         str
         reps))

(module+ test
  (test-case "replace-strings works"
    (check-equal? (replace-strings "hello mellow yellow fellow"
                                 '(("lo" "LO") ("el" "{{el}}")))
                "h{{el}}LO m{{el}}LOw y{{el}}LOw f{{el}}LOw")))

;; Format a list of expressions to a string, with one expression per line. The
;; list's parens aren't included.
(define (format-symbols syms)
  (string-join (map ~s syms) "\n"))

;; Print a list of expressions to (current-output-port)
(define (show x)
  (displayln (format-symbols x)))

;; Reverse the characters of a string
(define (string-reverse s)
  (list->string (reverse (string->list s))))

;; Reads a list of expressions from the given string
(define (read-benchmark x)
  (let* ([content (string-append "(\n" x "\n)")])
    (with-input-from-string content
      read)))

;; Prefix symbol N with string P
(define (prefix-name n p)
  (string->symbol (string-append p (symbol->string n))))

;; Apply F to each element of XS, and append the results together
(define (concat-map f xs)
  (append* (map f xs)))

(module+ test
  (test-case "Can concat-map"
    (check-equal? (concat-map (lambda (x) (list x x x))
                              '(fee fi fo fum))
                  '(fee fee fee fi fi fi fo fo fo fum fum fum))))

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

;; Returns the last N elements of LST
(define (take-from-end n lst)
  (reverse (take (reverse lst) n)))

(define (non-empty? x)
  (not (empty? x)))

;; Creates a list of pairs '((X1 Y1) (X2 Y2) ...) when given a pair of lists
;; '(X1 X2 ...) and '(Y1 Y2 ...)
(define (zip xs ys)
  (if (empty? xs)
      null
      (if (empty? ys)
          null
          (cons (list (car xs) (car ys))
                (zip  (cdr xs) (cdr ys))))))

;; Idempotent symbol->string
(define (as-str x)
  (if (string? x)
      x
      (symbol->string x)))

;; Returns TRUE if any element of XS passes predicate F, FALSE otherwise
(define (any-of f xs)
  (foldl (lambda (x y)
           (or (f x) y))
         #f
         xs))

;; Returns FALSE if any element of XS fails predicate F, TRUE otherwise
(define (all-of f xs)
  (foldl (lambda (x y)
           (and (f x) y))
         #t
         xs))

;; Map a function F over the elements of a set S
(define (map-set f s)
  (list->set (set-map s f)))

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

(define (assoc-get key val)
  (second (assoc key val)))

;; Deterministically, but unpredictably, shuffle the given NAMES. KEYGEN turns
;; a name into a hash, and we perform the shuffle by sorting the hashes.
(define (deterministic-shuffle keygen names)
  (define sorted
    (sort (map (lambda (n) (list n (keygen n)))
               names)
          (lambda (x y)
            (not (bytes>? (second x) (second y))))))
  (map first sorted))

(define lcm
  (lambda args
    (match args
      [(list x)             x]
      [(cons x (cons y zs)) (apply lcm (cons (* x (/ y (gcd x y)))
                                             zs))])))

(define (hash-foldl f init h)
  (foldl (lambda (kv result)
           (f (car kv) (cdr kv) result))
         init
         (hash->list h)))
