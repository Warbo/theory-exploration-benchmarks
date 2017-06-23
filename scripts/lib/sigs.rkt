#lang racket

(require shell/pipeline)
(require "impure.rkt")
(require "lists.rkt")
(require "normalise.rkt")
(require "tip.rkt")
(require "util.rkt")

(provide full-haskell-package)

(module+ test
  (require "testing.rkt")

  (define (string-to-haskell vals)
    ;; mk-final-defs takes in filenames, so it can qualify names. This makes
    ;; and cleans up temporary files for testing.

    ;; Note: We make a file in a directory, to avoid problems if tmpdir begins
    ;; with a number (e.g. '/var/run/user/1000'); otherwise qualified variable
    ;; names would be invalid
    (in-temp-dir (lambda (dir)
                   (define temp-file (string-append (path->string dir)
                                                    "/test.smt2"))

                   (for-each (lambda (val)
                               (display-to-file val  temp-file
                                                #:exists 'append)
                               (display-to-file "\n" temp-file
                                                #:exists 'append))
                             vals)

                   (let* ([result (defs-to-sig (list temp-file))])
                     (delete-file temp-file)
                     result)))))

;; Sends a string INPUT through the `tip` program for conversion to Haskell +
;; QuickSpec
(define (mk-signature-s input)
  (run-pipeline/out `(echo ,input)
                    '(tip --haskell-spec)))

(module+ test
  (def-test-case "Can't reference unbound names"
    (check-exn #rx"tip: Parse failed: Symbol bar .* not bound"
               (lambda ()
                 (mk-signature-s
                  (format-symbols
                   '((declare-datatypes
                      (a)
                      ((List (Nil) (Cons (head a) (tail (List a))))))
                     (define-fun
                       (par (a) (foo ((x (List a))) a
                                     (as (bar x) a))))
                     (check-sat)))))))

  (def-test-case "Can reference destructor names"
    (check-true (string? (mk-signature-s
                          (format-symbols
                           '((declare-datatypes
                              (a)
                              ((List (Nil) (Cons (head a) (tail (List a))))))
                             (define-fun
                               (par (a) (foo ((x (List a))) a
                                             (as (head x) a))))
                             (check-sat)))))))

  (def-test-case "Can't reuse destructor names"
    (check-exn #rx"tip: Parse failed: Symbol head .* is already globally bound"
               (lambda ()
                 (mk-signature-s
                  (format-symbols
                   '((declare-datatypes
                      (a)
                      ((List (Nil) (Cons (head a) (tail (List a))))))
                     (define-fun
                       (par (a) (head ((x (List a))) a
                                      (match x
                                        (case (Cons y zs) y)))))
                     (check-sat))))))))

;; Create a Haskell package in directory DIR containing rendered benchmark
;; definition STR
(define (full-haskell-package-s str dir)
  (define hs (mk-signature-s str))

  ;; Remove the generated signature, as it's incompatible with QuickSpec 1, and
  ;; remove the import of QuickSpec 2
  (define patched1
    (filter (lambda (line)
              (not (string-contains? line "import qualified QuickSpec as QS")))
            (takef (string-split hs "\n")
                   (lambda (line)
                     (not (string-prefix? line "sig ="))))))

  ;; Derive Generic instances for all types
  (define patched2
    (string-append "{-# LANGUAGE DeriveGeneric #-}\n"
                   (string-replace (string-join patched1 "\n")
                                   "deriving ("
                                   "deriving (GHC.Generics.Generic, ")))

  ;; Import GHC.Generics and Data.Serialize
  (define patched3
    (string-replace patched2
                    "import qualified Prelude as P"
                    "import qualified Prelude as P\nimport qualified GHC.Generics\nimport qualified Data.Serialize"))

  ;; Derive Serialize instances for all types. `tip` will give us instances like
  ;;
  ;;     instance QC.Arbitrary Nat where arbitrary = QC.sized F.uniform
  ;;     instance (F.Enumerable a) => QC.Arbitrary (List a) where
  ;;
  ;; We can use these as templates for our Data.Serialize.Serialize instances.
  ;; Note that the existing definitions may span multiple lines, so we insert
  ;; our new ones *before*, to avoid being in the middle of the existing one.
  (define patched4
    ;; Parameterised instance declarations. Note that .*? is non-greedy.
    (regexp-replace* #px"instance\\s+\\(F\\.Enumerable.*?where" patched3
                     (lambda (dec)
                       ;; Prepend new instance to existing dec
                       (string-append
                        ;; Removes the trailing "where" which we don't want
                        (string-trim
                         ;; Replace QC.Arbitrary and F.Enumerable
                         (string-replace
                          (string-replace dec
                                          "QC.Arbitrary"
                                          "Data.Serialize.Serialize")
                          "F.Enumerable"
                          "Data.Serialize.Serialize")
                         "where")
                        "\n"
                        dec))))
  (define patched5
    ;; Non-parameterised instance declarations. Again, .*? is non-greedy.
    (regexp-replace* #px"instance\\s+QC\\.Arbitrary.*?where" patched4
                     (lambda (dec)
                       ;; Prepend new instance to existing dec
                       (string-append
                        ;; Removes the trailing "where" which we don't want
                        (string-trim
                         ;; Replace QC.Arbitrary
                         (string-replace dec
                                         "QC.Arbitrary"
                                         "Data.Serialize.Serialize")
                         "where")
                        "\n"
                        dec))))

  (define patched patched5)

  (make-directory (string-append dir "/src"))

  (display-to-file patched (string-append dir "/src/A.hs"))
  (display-to-file "-- Initial tip-benchmark-sig.cabal generated by cabal init.  For further
-- documentation, see http://haskell.org/cabal/users-guide/

name:                tip-benchmark-sig
version:             0.1.0.1
synopsis:            Auto-generated package for theory exploration
-- description:
homepage:            http://example.org
license:             PublicDomain
license-file:        LICENSE
author:              Chris Warburton
maintainer:          chriswarbo@gmail.com
-- copyright:
category:            Testing
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10

library
  exposed-modules:     A
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.8
                     , quickspec
                     , QuickCheck
                     , testing-feat
                     , cereal
  hs-source-dirs:      src
  default-language:    Haskell2010
"
                   (string-append dir "/tip-benchmark-sig.cabal"))

  (display-to-file "Auto-generated from https://github.com/tip-org/benchmarks, the same LICENSE applies"
                   (string-append dir "/LICENSE")))

(module+ test
  (def-test-case "Module tests"
    (define files
      (string-join (theorem-files) "\n"))

    (in-temp-dir
     (lambda (dir)
       (define out-dir (path->string dir))
       (parameterize-env `([#"FILES"   ,(string->bytes/utf-8 files)]
                           [#"OUT_DIR" ,(string->bytes/utf-8 out-dir)])
         (lambda ()
           (full-haskell-package-s
            (format-symbols (mk-final-defs-hash (theorem-hashes)))
            out-dir)

           (with-check-info
            (('files   files)
             ('message "Made Haskell package"))
            (check-true (directory-exists? out-dir)))))

       (with-check-info
        (('message "Made src directory"))
        (check-true (directory-exists? (string-append out-dir "/src"))))

       (for-each (lambda (f)
                   (with-check-info
                    (('f       f)
                     ('message "Made package file"))
                    (check-true (file-exists? (string-append out-dir f)))))
                 '("/src/A.hs" "/tip-benchmark-sig.cabal" "/LICENSE"))

       (parameterize-env `([#"HOME" ,(string->bytes/utf-8 out-dir)])
         (lambda ()
           (define stdout (open-output-string))
           (define stderr (open-output-string))
           (define output "nothing")
           (parameterize ([current-directory out-dir]
                          [current-output-port stdout]
                          [current-error-port  stderr])
             (run-pipeline '(cabal configure))

             (set! output
               (run-pipeline/out '(echo -e "import A\n:browse")
                                 '(cabal repl -v0))))

           ;; If the import fails, we're stuck with the Prelude, which
           ;; contains classes like Functor
           (with-check-info
             (('output  output)
              ('stdout  (get-output-string stdout))
              ('stderr  (get-output-string stderr))
              ('message "Module imported successfully"))
             (check-false (string-contains? output "class Functor"))))))))

  (def-test-case "Haskell package made"
    (in-temp-dir
     (lambda (out-dir)
       (parameterize-env `([#"FILES"   ,(string->bytes/utf-8
                                         (string-join (theorem-files) "\n"))]
                           [#"OUT_DIR" ,(string->bytes/utf-8
                                         (path->string out-dir))]
                           [#"HOME"    ,(string->bytes/utf-8
                                         (path->string out-dir))])
         (lambda ()
           (full-haskell-package-s (format-symbols (mk-final-defs-hash (theorem-hashes)))
                                   (path->string out-dir))

           (parameterize ([current-directory out-dir])

             (check-true (directory-exists? "src"))

             (check-true (file-exists? "src/A.hs"))

             (check-true (file-exists? "tip-benchmark-sig.cabal"))

             (check-true (file-exists? "LICENSE"))

             (run-pipeline/out '(cabal configure))

             (define out
               (run-pipeline/out '(echo -e "import A\n:browse")
                                 '(cabal repl -v0)))

             ;; If the import fails, we're stuck with
             ;; the Prelude, which contains classes
             (check-false (string-contains? out
                                            "class Functor")))))))))

;; Commandline wrapper around full-haskell-package-s using stdio and env vars
(define (full-haskell-package)
  (full-haskell-package-s (port->string (current-input-port))
                          (getenv "OUT_DIR")))

(module+ test
  (define sig (string-to-haskell mut))

  (for-each
   (lambda (fun)
     (define def-found
       (any-of (lambda (line)
                 (and (string-prefix? line "global")
                      (let* ([id   (first (regexp-match "global[0-9a-f]+"
                                                        line))]
                             [enc  (substring id 6)]
                             [name (decode16 enc)])
                        (regexp-match? (string-append
                                        "^"
                                        (regexp-quote temp-file-prefix)
                                        "[0-9]+"
                                        (regexp-quote (string-append "/test.smt2" fun))
                                        "$")
                                       name))))
               (string-split sig "\n")))

     (with-check-info
      (('fun       fun)
       ('enc       (encode16 fun))
       ('def-found def-found)
       ('sig       sig)
       ('message   "Function defined in signature"))
      (check-true def-found)))
   (list "models" "models2" "models5")))

(define (defs-to-sig x)
  (mk-signature-s (format-symbols (mk-final-defs-hash
                                   (files-to-hashes x)))))

(module+ test
  (def-test-case "Single files"
    (define files (map (lambda (suf)
                         (benchmark-file suf))
                       '("tip2015/tree_sort_SortPermutes'.smt2")))

    (for-each (lambda (f)
                (define sig
                  (defs-to-sig (list f)))
                (with-check-info
                  (('sig sig))
                  (check-true (string-contains? sig "QuickSpec"))))
              files))

  (def-test-case "Multiple files"
    (define files
      (benchmark-files '("tip2015/tree_SwapAB.smt2"
                         "tip2015/list_SelectPermutations.smt2")))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('message "Local variables renamed")
      ('sig     sig))
     (check-true (string-contains? sig "local"))))

  (def-test-case "Random files"
    (define files
      (theorem-files))

    (define sig
      (defs-to-sig files))

    (with-check-info
     (('files   files)
      ('sig     sig)
      ('message "Made Haskell for random files"))
     (check-true (string-contains? sig "QuickSpec"))))

  (def-test-case "Form datatype survives translation"
    (define sig (string-to-haskell form-with-deps))

    (define data-found
      (regexp-match? "data[ ]+Global[0-9a-f]+[ ]+="
                     (string-replace sig "\n" "")))

    (with-check-info
     (('data-found data-found)
      ('sig        sig)
      ('message    "'Form' datatype appears in signature"))
     (check-true data-found)))

  ;; When TIP translates from its smtlib-like format to Haskell, it performs a
  ;; renaming step, to ensure that all names are valid Haskell identifiers. We
  ;; need to ensure that the names we produce don't get altered by this step.
  (def-test-case "Name preservation"
    (define test-benchmark-defs
      (mk-final-defs-hash (theorem-hashes)))

    (define test-benchmark-lower-names
      ;; A selection of names, which will be lowercase in Haskell
      (foldl (lambda (def result)
               (append result (lowercase-names def)))
             null
             test-benchmark-defs))

    (define test-benchmark-upper-names
      ;; A selection of names, which will be uppercase in Haskell
      (foldl (lambda (def result)
               (append result (uppercase-names def)))
             null
             test-benchmark-defs))

    (define (tip-lower-rename name)
      "Given a function name NAME, returns TIP's renamed version"

      (define input
        ;; A trivial definition which uses this name
        `((define-fun-rec ,name () Bool ,name)
          (check-sat)))

      (define output
        ;; Run through TIP
        (run-pipeline/out `(echo ,(format-symbols input))
                          '(tip --haskell)))

      ;; The definition will be on the only line with an "="
      (define def-line
        (first (filter (lambda (line)
                         (string-contains? line "="))
                       (string-split output "\n"))))

      ;; The name will be the only thing to the left of the "="
      (string-trim (first (string-split def-line "="))))

    (define (tip-upper-rename name)
      "Given a type name NAME, returns TIP's renamed version"

      (define input
        ;; A trivial definition which uses this name
        `((declare-datatypes () ((,name)))
          (check-sat)))

      (define output
        ;; Run through TIP
        (run-pipeline/out `(echo ,(format-symbols input))
                          '(tip --haskell)))

      ;; The definition will be immediately to the left of the only occurrence
      ;; of "="
      (define prefix
        (string-trim (first (string-split (string-replace output "\n" "")
                                          "="))))

      ;; The name will occur immediately after the final "data"
      (string-trim (last (string-split prefix "data"))))

    (set-for-each test-benchmark-lower-names
                  (lambda (name)
                    (check-equal? (tip-lower-rename name)
                                  (symbol->string name))))

    (set-for-each test-benchmark-upper-names
                  (lambda (name)
                    (check-equal? (tip-upper-rename name)
                                  (symbol->string name))))))
