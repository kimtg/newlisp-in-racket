#lang racket/base

(require racket/string
         racket/format
         racket/list
         racket/vector
         racket/math)

(provide (all-defined-out))

;; -------------------------------------------------------------------
;; Core Singletons: nil and true
;; -------------------------------------------------------------------

(struct nl-nil-type ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (display "nil" port))])

(struct nl-true-type ()
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (display "true" port))])

(define nl-nil (nl-nil-type))
(define nl-true (nl-true-type))

(define (nl-nil? v) (eq? v nl-nil))
(define (nl-true? v) (eq? v nl-true))

;; Boolean truthiness in newLISP:
;; Only `nil` and the empty list `()` are false.
;; Everything else is truthy.
(define (nl-truthy? v)
  (not (or (nl-nil? v)
           (null? v))))

(define (racket->nl-bool b)
  (if b nl-true nl-nil))

(define (nl-boolean? v)
  (or (nl-nil? v) (nl-true? v)))

;; -------------------------------------------------------------------
;; Symbols and Contexts
;; -------------------------------------------------------------------

(struct nl-symbol
  (context-name
   name
   [value #:mutable]
   [protected? #:mutable])
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (display (nl-symbol->display-string val) port))])

(struct nl-context
  (name
   symbols          ; hash: string -> nl-symbol
   [default-functor #:mutable]
   [protected? #:mutable])
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (display (nl-context-name val) port))])

(define (nl-symbol->display-string sym [current-ctx "MAIN"])
  (define ctx (nl-symbol-context-name sym))
  (if (or (string=? ctx "MAIN")
          (string=? ctx current-ctx))
      (nl-symbol-name sym)
      (string-append ctx ":" (nl-symbol-name sym))))

;; -------------------------------------------------------------------
;; Primitives (Built-in functions)
;; -------------------------------------------------------------------

(struct nl-primitive
  (name
   proc             ; (lambda (evaluator args ctx) ...)
   is-special?      ; #t if special form (unevaluated args)
   min-args
   max-args
   hex-id)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (fprintf port "~a <~a>" (nl-primitive-name val) (nl-primitive-hex-id val)))])

;; -------------------------------------------------------------------
;; Lambdas and Lambda-Macros (Fexprs)
;; -------------------------------------------------------------------

(struct nl-lambda
  (params           ; list of nl-symbol or (nl-symbol default-expr)
   body             ; list of expressions
   is-macro?        ; #t for lambda-macro
   ctx-name         ; context where defined
   [compiled-proc #:mutable #:auto]) ; compiled Racket procedure, if transpiled
  #:auto-value #f
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (display (nl->string val) port))])

;; In newLISP, lambda expressions can be accessed as lists of (params body...)
(define (nl-lambda->list lam)
  (cons (nl-lambda-params lam) (nl-lambda-body lam)))

;; -------------------------------------------------------------------
;; Arrays
;; -------------------------------------------------------------------

(struct nl-array
  (dims             ; list of exact positive integers, e.g. '(3 2)
   data)            ; flat vector of size (apply * dims)
  #:mutable
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc val port mode)
     (display (nl->string (nl-array->list val)) port))])

(define (make-nl-array dims [init '()])
  (define total (if (null? dims) 1 (apply * dims)))
  (define vec (make-vector total nl-nil))
  (define flat-init
    (let flatten-init ([x init])
      (cond
        [(null? x) '()]
        [(pair? x) (append (flatten-init (car x)) (flatten-init (cdr x)))]
        [(vector? x) (flatten-init (vector->list x))]
        [(nl-array? x) (vector->list (nl-array-data x))]
        [else (list x)])))
  (let loop ([i 0] [lst flat-init])
    (when (and (< i total) (pair? lst))
      (vector-set! vec i (car lst))
      (loop (+ i 1) (cdr lst))))
  (nl-array dims vec))

(define (nl-array-ref arr indices)
  (define dims (nl-array-dims arr))
  (define offset (calculate-array-offset dims indices))
  (vector-ref (nl-array-data arr) offset))

(define (nl-array-set! arr indices val)
  (define dims (nl-array-dims arr))
  (define offset (calculate-array-offset dims indices))
  (vector-set! (nl-array-data arr) offset val))

(define (calculate-array-offset dims indices)
  (unless (= (length dims) (length indices))
    (error 'array "dimension mismatch for indices: ~a for dims: ~a" indices dims))
  (let loop ([ds dims] [is indices] [offset 0])
    (if (null? ds)
        offset
        (let* ([d (car ds)]
               [idx (car is)]
               ;; Normalize negative index:
               [norm-idx (if (< idx 0) (+ d idx) idx)])
          (when (or (< norm-idx 0) (>= norm-idx d))
            (error 'array "index out of bounds: ~a (dim size ~a)" idx d))
          (loop (cdr ds)
                (cdr is)
                (+ (* offset d) norm-idx))))))

(define (nl-array->list arr)
  (define dims (nl-array-dims arr))
  (define data (nl-array-data arr))
  (define (build-nested ds start stride)
    (if (null? (cdr ds))
        (for/list ([i (car ds)])
          (vector-ref data (+ start (* i stride))))
        (let ([sub-stride (/ stride (car ds))])
          (for/list ([i (car ds)])
            (build-nested (cdr ds) (+ start (* i stride)) sub-stride)))))
  (if (null? dims)
      (vector-ref data 0)
      (let ([total (apply * dims)])
        (build-nested dims 0 (/ total (car dims))))))

;; -------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------

(define (nl-atom? v)
  (and (not (pair? v))
       (not (null? v))
       (not (nl-array? v))
       (not (nl-lambda? v))))

(define (nl-list? v)
  (or (null? v)
      (pair? v)
      (nl-lambda? v)))

(define (nl-string? v)
  (string? v))

(define (nl-number? v)
  (number? v))

(define (nl-integer? v)
  (exact-integer? v))

(define (nl-float? v)
  (and (real? v) (not (exact-integer? v))))

(define (nl-empty? v)
  (cond
    [(null? v) nl-true]
    [(string? v) (racket->nl-bool (= (string-length v) 0))]
    [(nl-array? v) (racket->nl-bool (= (apply * (nl-array-dims v)) 0))]
    [else nl-nil]))

;; -------------------------------------------------------------------
;; Conversion & String Formatting (nl->string)
;; -------------------------------------------------------------------

(define (nl->string v [escape-strings? #t] [current-ctx "MAIN"])
  (cond
    [(nl-nil? v) "nil"]
    [(nl-true? v) "true"]
    [(null? v) "()"]
    [(exact-integer? v) (number->string v)]
    [(real? v)
     (if (nan? v)
         "NaN"
         (if (infinite? v)
             (if (> v 0) "inf" "-inf")
             (let ([s (number->string (exact->inexact v))])
               ;; Normalize format
               (if (string-contains? s ".")
                   s
                   (string-append s ".0")))))]
    [(string? v)
     (if escape-strings?
         (string-append "\"" (escape-nl-string v) "\"")
         v)]
    [(nl-symbol? v)
     (nl-symbol->display-string v current-ctx)]
    [(nl-context? v)
     (nl-context-name v)]
    [(nl-primitive? v)
     (format "~a <~a>" (nl-primitive-name v) (nl-primitive-hex-id v))]
    [(nl-lambda? v)
     (format "(~a ~a ~a)"
             (if (nl-lambda-is-macro? v) "lambda-macro" "lambda")
             (nl->string (nl-lambda-params v) #t current-ctx)
             (string-join (map (lambda (e) (nl->string e #t current-ctx))
                               (nl-lambda-body v))
                          " "))]
    [(nl-array? v)
     (nl->string (nl-array->list v) escape-strings? current-ctx)]
    [(pair? v)
     (string-append "("
                    (string-join (let loop ([cur v])
                                   (cond
                                     [(null? cur) '()]
                                     [(pair? cur)
                                      (cons (nl->string (car cur) #t current-ctx)
                                            (loop (cdr cur)))]
                                     [else
                                      (list (nl->string cur #t current-ctx))]))
                                 " ")
                    ")")]
    [else (~a v)]))

(define (escape-nl-string s)
  (let ([out (open-output-string)])
    (for ([c (in-string s)])
      (case c
        [(#\\) (display "\\\\" out)]
        [(#\") (display "\\\"" out)]
        [(#\newline) (display "\\n" out)]
        [(#\return) (display "\\r" out)]
        [(#\tab) (display "\\t" out)]
        [(#\backspace) (display "\\b" out)]
        [(#\page) (display "\\f" out)]
        [else
         (let ([code (char->integer c)])
           (if (or (< code 32) (> code 126))
               (if (< code 256)
                   (fprintf out "\\~a" (~r code #:base 8 #:min-width 3 #:pad-string "0"))
                   (fprintf out "\\u~a" (~r code #:base 16 #:min-width 4 #:pad-string "0")))
               (display c out)))]))
    (get-output-string out)))
