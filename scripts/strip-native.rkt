#lang racket
(require racket/match)

(define (dump x)
  (write x (current-error-port))
  (display "\n" (current-error-port))
  x)

(define (start lst)
  (reverse (rest (reverse lst))))

;; Replacement definitions for boolean expressions, along with any dependencies
(define custom-bool
  (list
   '(declare-datatypes () ((CustomBool (CustomTrue) (CustomFalse))))
   '()))

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
      (custom-ite x y CustomFalse))
   (list custom-bool custom-ite)))

(define custom-or
  (list
   '(define-fun custom-or ((x CustomBool) (y CustomBool)) CustomBool
      (custom-ite x CustomTrue y))
   (list custom-bool custom-ite)))

;; NOTE: We can't just replace all occurrences of => with custom-=>, since that
;; symbol is also used for function types.
(define custom-=>
  (list
   '(define-fun custom-=> ((x CustomBool) (y CustomBool)) CustomBool
      (custom-ite x y CustomTrue))
   (list custom-bool custom-ite)))

;; NOTE: We must replace = to make conditional equations typecheck, e.g.
;; (custom-=> (= (foo x) (foo y)) (= (bar x) (bar y))), since = returns a
;; Bool and custom-=> expects CustomBools. However, = is polymorphic, which
;; we can't implement directly (e.g. using pattern matching), so the
;; definition of custom-= must use = internally (which also requires us to
;; eliminate the native Bool using ite)
(define custom-=
  (list
   '(define-fun (par (a) (custom-= ((x a) (y a)) CustomBool
                                   (ite (= x y) CustomTrue CustomFalse))))
   (list custom-bool)))

(define custom-distinct
  (list
   '(define-fun (par (a) (custom-distinct ((x a) (y a)) CustomBool
                                          (custom-not (custom-= x y)))))
   (list custom-= custom-not)))

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
      (custom-ite (custom-> (custom-abs y) (custom-abs x))
                  CustomZero
                  (custom-* (custom-sign x)
                            (custom-* (custom-sign y)
                                      (custom-inc (custom-div (custom-- (custom-abs x)
                                                                        (custom-abs y))
                                                              (custom-abs y)))))))
   (list custom-int custom-ite custom-> custom-abs custom-* custom-sign
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
;; function types (=>) being treated as boolean implication (=>).
(define (replace-native expr type-level?)
  (match expr

    ;; Booleans
    ['Bool  (list 'CustomBool  (list custom-bool))]
    ['true  (list 'CustomTrue  (list custom-bool))]
    ['false (list 'CustomFalse (list custom-bool))]

    ['ite   (list 'custom-ite  (list custom-ite))]

    ['and   (list 'custom-and  (list custom-and))]

    ['or    (list 'custom-or   (list custom-or))]

    ['not   (list 'custom-not  (list custom-not))]

    ;; Note that the definition of custom-= uses =, so we shouldn't e.g. apply
    ;; replace-native to the definition of custom-=.
    ['=     (list 'custom-=    (list custom-=))]

    ['distinct (list 'custom-distinct (list custom-distinct))]

    ;; Only swap => when used as a value, not a type
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
    [(list 'lambda args body)  (let ([args2 (replace-native args #t)]
                                     [body2 (replace-native body #f)])
                                 (list `(lambda ,(first args2) ,(first body2))
                                       (append (second args2) (second body2))))]

    [(list 'forall args body)  (let ([args2 (replace-native args #t)]
                                     [body2 (replace-native body #f)])
                                 (list `(forall ,(first args2) ,(first body2))
                                       (append (second args2) (second body2))))]

    [(list 'as v t) (let ([v2 (replace-native v #f)]
                          [t2 (replace-native t #t)])
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
         (define arg-type2 (replace-native (second destructor) #t))
         (list (list name (first arg-type2))
               (second arg-type2)))

       (match (foldl replace-in-type
                     '(() ())
                     types)
         [(list types2 deps)
          (list `(declare-datatypes ,params ,types2)
                deps)]))]

    ;; Recurse through structures
    [(cons x y) (let ([x2 (replace-native x type-level?)]
                      [y2 (replace-native y type-level?)])
                  (list (cons (first x2) (first y2))
                        (append (second x2) (second y2))))]

    ;; Catch-all
    [_ (list expr '())]))

;; Prevents repeating ourselves
(define (replace-in-func args return body)
  (define args2   (replace-native args   #t))
  (define return2 (replace-native return #t))
  (define body2   (replace-native body   #f))
  (list (first  args2) (first  return2) (first  body2)
        (append (second args2) (second return2) (second body2))))

;; Replace native definitions and prepend any newly required definitions
(define (replace-all exprs)
  (define replaced
    (replace-native exprs #f))
  (define raw-deps
    (dependencies-closure (list 'te-sentinel-value (second replaced))))

  (remove-duplicates
   (remove 'te-sentinel-value
           (append raw-deps (first replaced)))))

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
            (define result (replace-all raw))

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
