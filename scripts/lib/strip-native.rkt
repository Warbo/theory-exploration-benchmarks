#lang racket

;; Since this module is used (among other things) to generate BENCHMARKS, it
;; shouldn't depend on anything which needs to read cached data from the
;; environment.

(require racket/match)

(provide benchmark-tests custom-bool custom-ite custom-not custom-and custom-or
         custom-=> custom-bool-converter custom-nat custom-int custom-plus
         custom-inc custom-dec custom-invert custom-abs custom-sign custom-+
         custom-- custom-* custom-nat-> custom-> custom-div custom-mod custom-<
         custom->= custom-<= mk-source process-tip-file! replace-all-native
         start tip-files-in)

(define (dump x)
  (write x (current-error-port))
  (display "\n" (current-error-port))
  x)

;; Opposite of rest/cdr: returns all elements of a list except the last one.
;; Like Haskell's "init".
(define (start lst)
  (reverse (rest (reverse lst))))

;; Replacement definitions for boolean expressions, along with any dependencies
(define custom-bool
  (list
   '(declare-datatypes () ((CustomBool (CustomTrue) (CustomFalse))))
   '()))

;; Note that we can't use custom-ite as a general replacement for if/then/else
;; or match/case, since strict languages will evaluate both branch arguments,
;; which will cause recursive definitions to diverge.
(define custom-ite
  (list
   '(define-fun
      (par (a)
           (custom-ite
            ((c CustomBool) (x a) (y a)) a
            (match c
              (case CustomTrue  x)
              (case CustomFalse y)))))
   (list custom-bool)))

(define custom-not
  (list
   '(define-fun custom-not ((x CustomBool)) CustomBool
      (custom-ite x CustomFalse CustomTrue))
   (list custom-bool custom-ite)))

(define custom-and
  (list
   '(define-fun custom-and ((x CustomBool) (y CustomBool)) CustomBool
      (match x
        (case CustomTrue  y)
        (case CustomFalse CustomFalse)))
   (list custom-bool)))

(define custom-or
  (list
   '(define-fun custom-or ((x CustomBool) (y CustomBool)) CustomBool
      (match x
        (case CustomTrue  CustomTrue)
        (case CustomFalse y)))
   (list custom-bool)))

;; NOTE: We can't just replace all occurrences of => with custom-=>, since that
;; symbol is also used for function types.
(define custom-=>
  (list
   '(define-fun custom-=> ((x CustomBool) (y CustomBool)) CustomBool
      (match x
        (case CustomTrue  y)
        (case CustomFalse CustomTrue)))
   (list custom-bool)))

;; NOTE: We must replace = to make conditional equations typecheck, e.g.
;; (custom-=> (= foo bar) (= foo baz)), since = returns a Bool and custom-=>
;; expects CustomBools. However, we can't write our own custom-= function, since
;; it needs to be polymorphic, and Haskell will complain that we can't use `==`
;; without adding an `Eq a` constraint to our polymorphic type parameter, but
;; TIP doesn't provide any way to do that.

;; We work around this by using the existing `=`, but wrapping each usage in a
;; call to custom-bool-converter, so the above example would become:
;;
;; (custom-=> (custom-bool-converter (= foo bar))
;;            (custom-bool-converter (= foo baz)))

;; This way, if the arguments of = have concrete types, their implementation of
;; Eq will be used directly, no constraints have to be propagated and Haskell
;; won't complain about any being missing.
(define custom-bool-converter
  (list
   '(define-fun custom-bool-converter ((x Bool)) CustomBool
      (ite x CustomTrue CustomFalse))
   (list custom-bool)))

;; Used in CustomInt; will probably overlap with a benchmark's definition, but
;; we will strip out redundancies anyway
(define custom-nat
  (list
   '(declare-datatypes () ((CustomNat (CustomZ)
                                      (CustomS (custom-p CustomNat)))))
   '()))

;; Replacement definitions for integer expressions. There are several
;; representations we could have used, this was chosen for its symmetry and for
;; giving each integer a single representation (in particular, no -0)
(define custom-int
  (list
   '(declare-datatypes () ((CustomInt (CustomNeg (custom-succ CustomNat))
                                      (CustomZero)
                                      (CustomPos (custom-pred CustomNat)))))
   (list custom-nat)))

;; Converts integer literals to our custom datatype
(define (int->custom x)
  (define (int->nat x)
    (if (equal? x 0)
        'CustomZ
        `(CustomS ,(int->nat (- (abs x) 1)))))

  (cond
    [(equal? x 0) 'CustomZero]
    [(<      x 0) `(CustomNeg ,(int->nat (- (abs x) 1)))]
    [(>      x 0) `(CustomPos ,(int->nat (- (abs x) 1)))]))

(define custom-plus
  (list
   '(define-fun-rec custom-plus ((x CustomNat) (y CustomNat)) CustomNat
      (match x
        (case  CustomZ y)
        (case (CustomS x2) (custom-plus x2 (CustomS y)))))
   (list custom-nat)))

(define custom-inc
  (list
   '(define-fun-rec custom-inc ((x CustomInt)) CustomInt
      (match x
        (case  CustomZero     (CustomPos CustomZ))
        (case (CustomPos  x2) (CustomPos (CustomS x2)))
        (case (CustomNeg  x2) (match x2
                                (case  CustomZ      CustomZero)
                                (case (CustomS x3) (CustomNeg x3))))))
   (list custom-int)))

(define custom-dec
  (list
   '(define-fun-rec custom-dec ((x CustomInt)) CustomInt
      (match x
        (case  CustomZero    (CustomNeg  CustomZ))
        (case (CustomPos x2) (match x2
                               (case  CustomZ      CustomZero)
                               (case (CustomS x3) (CustomPos x3))))
        (case (CustomNeg x2) (CustomNeg (CustomS x2)))))
   (list custom-int)))

(define custom-invert
  (list
   '(define-fun-rec custom-invert ((x CustomInt)) CustomInt
      (match x
        (case  CustomZero     x)
        (case (CustomPos x2) (CustomNeg x2))
        (case (CustomNeg x2) (CustomPos x2))))
   (list custom-int)))

(define custom-abs
  (list
   '(define-fun-rec custom-abs ((x CustomInt)) CustomInt
      (match x
        (case  CustomZero x)
        (case (CustomPos x2) (CustomPos x2))
        (case (CustomNeg x2) (CustomPos x2))))
   (list custom-int)))

(define custom-sign
  (list
   '(define-fun-rec custom-sign ((x CustomInt)) CustomInt
      (match x
        (case  CustomZero x)
        (case (CustomPos x2) (CustomPos CustomZ))
        (case (CustomNeg x2) (CustomNeg CustomZ))))
   (list custom-int)))

(define custom-+
  (list
   '(define-fun-rec custom-+ ((x CustomInt) (y CustomInt)) CustomInt
      (match x
        (case  CustomZero     y)
        (case (CustomPos x2) (custom-+ (custom-dec x) (custom-inc y)))
        (case (CustomNeg x2) (custom-+ (custom-inc x) (custom-dec y)))))
   (list custom-int custom-inc custom-dec)))

(define custom--
  (list
   '(define-fun-rec custom-- ((x CustomInt) (y CustomInt)) CustomInt
      (custom-+ x (custom-invert y)))
   (list custom-int custom-+ custom-invert)))

(define custom-*
  (list
   '(define-fun-rec custom-* ((x CustomInt) (y CustomInt)) CustomInt
      (match x
        (case  CustomZero     CustomZero)
        (case (CustomPos x2) (custom-+ y (custom-* (custom-dec x) y)))
        (case (CustomNeg x2) (custom-invert (custom-* (CustomPos x2) y)))))
   (list custom-int custom-+ custom-invert custom-dec)))

(define custom-nat->
  (list
   '(define-fun-rec custom-nat-> ((x CustomNat) (y CustomNat)) CustomBool
      (match x
        (case  CustomZ     CustomFalse)
        (case (CustomS x2) (match y
                             (case  CustomZ      CustomTrue)
                             (case (CustomS y2) (custom-nat-> x2 y2))))))
   (list custom-nat custom-bool)))

(define custom->
  (list
   '(define-fun-rec custom-> ((x CustomInt) (y CustomInt)) CustomBool
      (match x
        (case  CustomZero    (match y
                               (case  CustomZero     CustomFalse)
                               (case (CustomPos y2)  CustomFalse)
                               (case (CustomNeg y2)  CustomTrue)))
        (case (CustomPos x2) (match y
                               (case  CustomZero     CustomTrue)
                               (case (CustomPos y2) (custom-nat-> x2 y2))
                               (case (CustomNeg y2)  CustomTrue)))
        (case (CustomNeg x2) (match y
                               (case  CustomZero     CustomFalse)
                               (case (CustomPos y2)  CustomFalse)
                               (case (CustomNeg y2) (custom-nat-> y2 x2))))))
   (list custom-int custom-bool custom-nat->)))

(define custom-div
  (list
   '(define-fun-rec custom-div ((x CustomInt) (y CustomInt)) CustomInt
      (match (custom-> (custom-abs y) (custom-abs x))
        (case CustomTrue  CustomZero)
        (case CustomFalse (custom-*
                           (custom-sign x)
                           (custom-*
                            (custom-sign y)
                            (custom-inc (custom-div (custom-- (custom-abs x)
                                                              (custom-abs y))
                                                    (custom-abs y))))))))
   (list custom-bool custom-int custom-> custom-abs custom-* custom-sign
         custom-inc custom--)))

(define custom-mod
  (list
   '(define-fun-rec custom-mod ((x CustomInt) (y CustomInt)) CustomInt
      (custom-- x (custom-* y (custom-div x y))))
   (list custom-int custom-- custom-* custom-div)))

(define custom-<
  (list
   '(define-fun-rec custom-< ((x CustomInt) (y CustomInt)) CustomBool
      (custom-> y x))
   (list custom-int custom-bool custom->)))

(define custom->=
  (list
   '(define-fun-rec custom->= ((x CustomInt) (y CustomInt)) CustomBool
      (custom-not (custom-< x y)))
   (list custom-int custom-bool custom-not custom-<)))

(define custom-<=
  (list
   '(define-fun-rec custom-<= ((x CustomInt) (y CustomInt)) CustomBool
      (custom-not (custom-> x y)))
   (list custom-int custom-bool custom-not custom->)))

;; Makes a list of all definitions depended on based on the given pair
;; (expr (dep1 dep2 ...)), where each dep is also such a pair.
(define (dependencies-closure x)
  (match x
    [(list expr deps) (let ([rec (apply append (map dependencies-closure deps))])
                        (remove-duplicates (append rec (list expr))))]))

;; Replaces native expressions, returning '(new-expr new-deps) where new-deps
;; includes any dependencies required for the replacement. The type-level?
;; argument tells us whether expr is a type or a value; it's used to prevent
;; function types (=>) being treated as boolean implication (=>). The
;; in-conjecture? argument tells us whether expr appears in a definition or a
;; conjecture; in the latter case, TIP treats = and => very differently.
(define (replace-native expr type-level? in-conjecture?)
  (match expr

    ;; Booleans
    ['Bool  (list 'CustomBool  (list custom-bool))]
    ['true  (list 'CustomTrue  (list custom-bool))]
    ['false (list 'CustomFalse (list custom-bool))]

    ['ite   (list 'custom-ite  (list custom-ite))]

    ['and   (list 'custom-and  (list custom-and))]

    ['or    (list 'custom-or   (list custom-or))]

    ['not   (list 'custom-not  (list custom-not))]

    ;; We can't make our own polymorphic =, since the necessary Eq constraints
    ;; won't be propagated when translated to Haskell. Instead, we manipulate
    ;; the call sites of the existing = function, to convert the result into a
    ;; CustomBool.
    [`(= ,x ,y) (let ([x2 (replace-native x type-level? in-conjecture?)]
                      [y2 (replace-native y type-level? in-conjecture?)])
                  (list `(custom-bool-converter (= ,(first x2) ,(first y2)))
                        (append (list custom-bool-converter)
                                (second x2)
                                (second y2))))]

    ;; Similar to =, we wrap call sites of distinct to use CustomBool
    [`(distinct ,x ,y) (let ([x2 (replace-native x type-level? in-conjecture?)]
                             [y2 (replace-native y type-level? in-conjecture?)])
                         (list `(custom-bool-converter (distinct ,(first x2)
                                                                 ,(first y2)))
                               (append (list custom-bool-converter)
                                       (second x2)
                                       (second y2))))]

    ;; => is used for function types, which should not be swapped out, and
    ;; boolean implication, which should be swapped out.
    ['=>    (if type-level?
                (list '=> '())
                (list 'custom-=> (list custom-=>)))]

    ;; Integers
    ['Int         (list 'CustomInt         (list custom-int))]
    [(? integer?) (list (int->custom expr) (list custom-int))]
    ['+           (list 'custom-+   (list custom-+))]
    ['-           (list 'custom--   (list custom--))]
    ['*           (list 'custom-*   (list custom-*))]
    ['div         (list 'custom-div (list custom-div))]
    ['mod         (list 'custom-mod (list custom-mod))]
    ['<           (list 'custom-<   (list custom-<))]
    ['>           (list 'custom->   (list custom->))]
    ['<=          (list 'custom-<=  (list custom-<=))]
    ['>=          (list 'custom->=  (list custom->=))]

    ;; Cases where we need to switch into type mode
    [(list 'lambda args body)  (let ([args2 (replace-native args #t in-conjecture?)]
                                     [body2 (replace-native body #f in-conjecture?)])
                                 (list `(lambda ,(first args2) ,(first body2))
                                       (append (second args2) (second body2))))]

    [(list 'forall args body)  (let ([args2 (replace-native args #t in-conjecture?)]
                                     [body2 (replace-native body #f in-conjecture?)])
                                 (list `(forall ,(first args2) ,(first body2))
                                       (append (second args2) (second body2))))]

    [(list 'as v t) (let ([v2 (replace-native v #f in-conjecture?)]
                          [t2 (replace-native t #t in-conjecture?)])
                      (list `(as ,(first v2) ,(first t2))
                            (append (second v2) (second t2))))]

    [`(define-fun-rec (par ,p (,name ,args ,return ,body)))
     (match (replace-in-func args return body)
       [(list args2 return2 body2 deps)
        (list `(define-fun-rec (par ,p (,name ,args2 ,return2 ,body2)))
              deps)])]

    [`(define-fun-rec ,name ,args ,return ,body)
     (match (replace-in-func args return body)
       [(list args2 return2 body2 deps)
        (list `(define-fun-rec ,name ,args2 ,return2 ,body2)
              deps)])]

    [`(define-funs-rec ,decs ,defs)
     (match (foldl (lambda (index others)
                     (define dec (list-ref decs index))
                     (match (replace-in-func (second dec)
                                             (third  dec)
                                             (list-ref defs index))
                       [(list args2 return2 body2 deps)
                        (list (append (first others)
                                      (list `(,(first dec) ,args2 ,return2)))
                              (append (second others)
                                      (list body2))
                              (append (third others)
                                      deps))]))
                   '(() () ())
                   (range (length decs)))
       [(list decs2 defs2 deps)
        (list `(define-funs-rec ,decs2 ,defs2)
              deps)])]

    [`(define-fun (par ,p (,name ,args ,return ,body)))
     (match (replace-in-func args return body)
       [(list args2 return2 body2 deps)
        (list `(define-fun (par ,p (,name ,args2 ,return2 ,body2)))
              deps)])]

    [`(define-fun ,name ,args ,return ,body)
     (match (replace-in-func args return body)
       [(list args2 return2 body2 deps)
        (list `(define-fun ,name ,args2 ,return2 ,body2)
              deps)])]

    [`(declare-datatypes ,params ,types)
     (let ()
       (define (replace-in-type type others)
         ;; Recurse through all constructors
         (define type-name (first type))
         (match (foldl (lambda (constructor others)
                         (define rec (replace-in-constructor constructor))
                         (list (append (first others)
                                       (list (first rec)))
                               (append (second others)
                                       (second rec))))
                       '(() ())
                       (cdr type))
           [(list constructors2 deps)
            (list (append (first others)
                          (list (cons type-name constructors2)))
                  (append (second others)
                          deps))]))

       (define (replace-in-constructor constructor)
         ;; Recurse through all destructors, if any
         (define constructor-name (first constructor))
         (match (foldl (lambda (destructor others)
                         (define rec (replace-in-destructor destructor))
                         (list (append (first others)
                                       (list (first rec)))
                               (append (second others)
                                       (second rec))))
                       '(() ())
                       (cdr constructor))
           [(list destructors2 deps)
            (list (cons constructor-name destructors2)
                  deps)]))

       (define (replace-in-destructor destructor)
         (define name      (first destructor))
         (define arg-type2 (replace-native (second destructor) #t in-conjecture?))
         (list (list name (first arg-type2))
               (second arg-type2)))

       (match (foldl replace-in-type
                     '(() ())
                     types)
         [(list types2 deps)
          (list `(declare-datatypes ,params ,types2)
                deps)]))]

    ;; Switches to in-conjecture mode
    [(list 'assert-not body) (let ([body2 (replace-native body #f #t)])
                               (list `(assert-not ,(first body2))
                                     (second body2)))]

    ;; Recurse through structures
    [(cons x y) (let ([x2 (replace-native x type-level? in-conjecture?)]
                      [y2 (replace-native y type-level? in-conjecture?)])
                  (list (cons (first x2) (first y2))
                        (append (second x2) (second y2))))]

    ;; Catch-all
    [_ (list expr '())]))

;; Prevents repeating ourselves
(define (replace-in-func args return body)
  (define args2   (replace-native args   #t #f))
  (define return2 (replace-native return #t #f))
  (define body2   (replace-native body   #f #f))
  (list (first  args2) (first  return2) (first  body2)
        (append (second args2) (second return2) (second body2))))

;; Replace native definitions and prepend any newly required definitions
(define (replace-all-native exprs)
  (define replaced
    (replace-native exprs #f #f))
  (define raw-deps
    (dependencies-closure (list 'te-sentinel-value (second replaced))))

  (remove-duplicates
   (remove 'te-sentinel-value
           (append raw-deps (first replaced)))))

(define (mk-source repo-dir)
  (string-append repo-dir "/benchmarks"))

(define (is-tip? x)
  (string-suffix? x ".smt2"))

(define ((in-source source) x)
  (string-trim (path->string x)
               (string-append source "/")
               #:left?  #t
               #:right? #f))

(define (tip-files-in source)
  (filter is-tip? (map (in-source source)
                       (sequence->list (in-directory source)))))

(define (read-source source f)
  (read (open-input-string
         (string-append "(\n"
                        (file->string (string-append source "/" f))
                        "\n)"))))

(define (destination-path destination f)
  (apply build-path
         (cons destination (start (explode-path f)))))

(define (get-result source f)
  (string-join (map (curry format "~s")
                    (replace-all-native (read-source source f)))
               "\n"))

(define ((process-tip-file! source destination) f)
  (eprintf (format "Stripping native symbols from ~a\n" f))

  ;; Read in the raw TIP benchmark, as a list of s-expressions,
  ;; and replace int, bool, etc. with our custom versions.

  (define result
    (get-result source f))

  ;; Write out the altered s-expression
  (make-directory* (destination-path destination f))

  (with-output-to-file (string-append destination "/" f)
    (lambda () (display result))
    #:exists 'replace))

;; Tests

(define (err . args)
  (error (format "~s" args)))

(define (same-files? source dest)
  (define in-source (sort (tip-files-in source) string<=?))
  (define in-dest   (sort (tip-files-in dest)   string<=?))
  (unless (equal? in-source in-dest)
    (err `((error     "Different file lists")
           (in-source ,in-source)
           (in-dest   ,in-dest)))))

(define (no-native-symbols dest)
  (define name-chars
    "[^><=a-zA-Z0-9-]")

  (for-each
   (lambda (f)
     (for-each
      (lambda (symbol)
        ;; Look for this operator, but avoid matching parts of other symbols
        ;; (e.g. thinking that "opposite" is "ite") by disallowing
        ;; characters before/after which are valid in names. Note that we
        ;; don't check the definition of custom-bool-converter since ite and
        ;; Bool are unavoidable there.
        (for-each
         (lambda (line)
           (unless (regexp-match "[(]define-fun custom-bool-converter " line)
             (when (regexp-match (string-append name-chars symbol name-chars)
                                 line)
               (err `((error  "Operator should have been replaced")
                      (symbol ,symbol)
                      (file   ,f))))))
         (file->lines (string-append dest "/" f))))

      ;; We can't check for '=>' since it's both implication and a function
      ;; type
      '("ite" "and" "false" "not" "or" "true" "True" "False" "Bool" "Int"
        "[+]" "[*]" "div" "mod" ">" "<" ">=" "<=")))
   (tip-files-in dest)))

(define (benchmark-tests source dest)
  (eprintf "Checking results\n")

  (same-files? source dest)

  (no-native-symbols dest))
