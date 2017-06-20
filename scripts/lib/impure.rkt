#lang racket

(require shell/pipeline)
(require "memo.rkt")
(require "util.rkt")

(provide benchmark-file benchmark-files benchmark-dir in-temp-dir msg
         parameterize-env pipe quiet read-from-cache!
         set-theorem-files! theorem-files temp-file-prefix)

(module+ test
  ;; Don't use testing.rkt, as that would cause a circular dependency
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

;; Define a 'msg' function for printing progress info to stderr. We also define
;; a 'quiet' function for turning off progress info.
(define-values (quiet msg)
  (let ([verbose #t])
    (values (lambda ()
              (unless (getenv "DEBUG")
                (set! verbose #f)))

            (lambda args
              (when verbose
                (eprintf (apply format args)))))))

(define benchmark-dir
  (or (getenv "BENCHMARKS")
      (getenv "BENCHMARKS_FALLBACK")
      (raise-user-error
       'benchmark-dir
       "No BENCHMARKS_FALLBACK env var found; should be set by Nix")))

;; Use this thunk to find the set of paths we're benchmarking. By default, use
;; all  files in benchmark-dir.
(memo0 theorem-files
       (sort (map path->string
                  (filter (lambda (x) (string-suffix? (path->string x) ".smt2"))
                          (sequence->list (in-directory benchmark-dir))))
             string<=?))


;; Override the theorem files to be used. If you're going to use this, do it
;; before computing anything, to prevent stale values being memoised. Basically
;; only exists for making the test suite quicker.
;;
;; REMEMBER: theorem-files should be a thunk returning a list, not a list!
(define (set-theorem-files! proc)
  (set! theorem-files (lambda ()
                        (sort (proc) string<=?))))

(define benchmark-file
  (curry string-append benchmark-dir "/"))

(define benchmark-files
  (curry map benchmark-file))

(define (read-from-cache! cache fallback)
  (if (and (getenv "IN_TEST")
           (not (getenv "BENCHMARKS_TEST_ALL")))
      (fallback)
      (let ()
        (define cache-path (getenv cache))

        (if (and cache-path (file-exists? cache-path))
            (let ()
              (define in   (open-input-file cache-path))
              (define data (read in))
              (close-input-port in)
              data)
            (error (string-append "No " cache " given"))))))
