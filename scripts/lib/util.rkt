#lang racket

(provide any->bool as-str
         decode16 define/test-contract
         encode16 format-symbols hash-foldl list->lines
         map-set prefix-name
         read-benchmark replace-strings
         show string-reverse)

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
(define (list->lines lst)
  (string-join (map ~s lst) "\n"))

(define format-symbols list->lines)

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

;; Idempotent symbol->string
(define (as-str x)
  (if (string? x)
      x
      (symbol->string x)))

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

(define/test-contract (any->bool x)
  (-> any/c boolean?)
  (not (not x)))
