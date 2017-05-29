#lang racket

(require shell/pipeline)
(require "util.rkt")

(provide in-temp-dir parameterize-env pipe temp-file-prefix)

(module+ test
  (require rackunit))

;; Run F with the string S as its input port. Returns whatever F writes to its
;; output port.
(define (pipe s f)
  (define o (open-output-string))
  (parameterize ([current-input-port  (open-input-string s)]
                 [current-output-port o])
    (f))
  (get-output-string o))

;; Run BODY with VARS present in the environment variables
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
  (test-case "Can parameterise env vars"

    (let ([a (getenv "HOME")]
          [b (parameterize-env '([#"HOME" #"foo"])
                               (lambda () (getenv "HOME")))]
          [c (getenv "HOME")])
      (check-equal? a c)
      (check-equal? b "foo"))))

;; Prefix used to identify temporary files as ours
(define temp-file-prefix
  "tebenchmarktemp")

(define (in-temp-dir f)
  (let* ([dir    (make-temporary-file (string-append temp-file-prefix "~a")
                                      'directory)]
         [result (f dir)])
    (delete-directory/files dir)
    result))

;; Reads a list of expressions from the given file
(define (file->list f)
  (read-benchmark (file->string f)))

;; Debug dump to stderr
(define (dump x)
  (write x (current-error-port))
  x)

;; Print a list of expressions to (current-output-port)
(define (show x)
  (displayln (format-symbols x)))
