#lang racket

(require lib/strip-native)

(define destination
  (getenv "DESTINATION"))

(when (member destination '("" #f))
  (error "No DESTINATION given"))

(define source
  (getenv "SOURCE"))

(when (member source '("" #f))
  (error "No SOURCE given"))

(define input-files
  (filter (lambda (x) (string-suffix? x ".smt2"))
          (map (lambda (x)
                 (string-trim (path->string x)
                              (string-append source "/")
                              #:left? #t
                              #:right? #f))
               (sequence->list (in-directory source)))))

(for-each (lambda (f)
            (display (format "Stripping native symbols from ~a\n" f)
                     (current-error-port))
            ;; Read in the raw TIP benchmark, as a list of s-expressions
            (define input
              (string-append "(\n"
                             (file->string (string-append source "/" f))
                             "\n)"))

            (define raw
              (let ([in (open-input-string input)])
                (read in)))

            ;; Write out the replaced versions, unwrapping the list
            (define result (replace-all-native raw))

            (make-directory* (apply build-path (cons destination
                                                       (start (explode-path f)))))
            (let ([out-string (open-output-string)]
                  [out-file   (open-output-file (string-append destination
                                                               "/"
                                                               f)
                                                #:exists 'replace)])
              (for-each (lambda (expr)
                          (write expr out-string)
                          (display "\n" out-string))
                        result)
              (display (get-output-string out-string) out-file)
              (close-output-port out-file)))
          input-files)
