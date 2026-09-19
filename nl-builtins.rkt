#lang racket/base

(require racket/string
         racket/list
         racket/hash
         racket/format
         racket/math
         racket/port
         racket/system
         racket/file
         racket/date
         net/url
         net/http-client
         json
         xml
         "nl-types.rkt"
         "nl-reader.rkt"
         "nl-eval.rkt"
         "nl-http.rkt"
         "nl-files.rkt"
         "nl-math-ext.rkt"
         "nl-net.rkt"
         "nl-ext.rkt")

(provide (all-defined-out))

(define (register-primitive! name proc [is-special? #f] [min-args 0] [max-args #f])
  (define hex-id (~r (abs (equal-hash-code name)) #:base 16 #:min-width 8 #:pad-string "0"))
  (define prim (nl-primitive name proc is-special? min-args max-args hex-id))
  (define sym (find-or-create-symbol name main-context))
  (set-symbol-val! sym prim)
  prim)

(define (reg-prim! name proc [is-special? #f] [min-args 0] [max-args #f])
  (register-primitive! name proc is-special? min-args max-args)
  (void))

(define (register-alias! alias-name prim)
  (define sym (find-or-create-symbol alias-name main-context))
  (set-symbol-val! sym prim))

;; -------------------------------------------------------------------
;; Arithmetic & Math
;; -------------------------------------------------------------------

;; + / add
(define prim-add
  (register-primitive! "+"
    (lambda (evaluator args ctx)
      (if (null? args) 0 (apply + args)))))
(register-alias! "add" prim-add)

;; - / sub
(define prim-sub
  (register-primitive! "-"
    (lambda (evaluator args ctx)
      (cond
        [(null? args) 0]
        [(= (length args) 1) (- (car args))]
        [else (apply - args)]))))
(register-alias! "sub" prim-sub)

;; * / mul
(define prim-mul
  (register-primitive! "*"
    (lambda (evaluator args ctx)
      (if (null? args) 1 (apply * args)))))
(register-alias! "mul" prim-mul)

;; / / div
(define prim-div
  (register-primitive! "/"
    (lambda (evaluator args ctx)
      (cond
        [(null? args) 1]
        [(= (length args) 1) (/ 1 (car args))]
        [else (apply / args)]))))
(register-alias! "div" prim-div)

;; % / mod
(define prim-mod
  (reg-prim! "%"
    (lambda (evaluator args ctx)
      (cond
        [(null? args) 0]
        [(= (length args) 1) (car args)]
        [else (modulo (car args) (cadr args))]))))
(register-alias! "mod" prim-mod)

;; ++ (increment integer place)
(reg-prim! "++"
  (lambda (evaluator args ctx)
    (define place-expr (car args))
    (define delta (if (pair? (cdr args)) (inexact->exact (truncate (evaluator (cadr args)))) 1))
    (define cur (evaluator place-expr))
    (define num (if (nl-nil? cur) 0 (inexact->exact (truncate cur))))
    (define new-val (+ num delta))
    (mutate-place! place-expr new-val)
    new-val)
  #t) ; special form: place unevaluated

;; -- (decrement integer place)
(reg-prim! "--"
  (lambda (evaluator args ctx)
    (define place-expr (car args))
    (define delta (if (pair? (cdr args)) (inexact->exact (truncate (evaluator (cadr args)))) 1))
    (define cur (evaluator place-expr))
    (define num (if (nl-nil? cur) 0 (inexact->exact (truncate cur))))
    (define new-val (- num delta))
    (mutate-place! place-expr new-val)
    new-val)
  #t)

;; inc (increment float place)
(reg-prim! "inc"
  (lambda (evaluator args ctx)
    (define place-expr (car args))
    (define delta (if (pair? (cdr args)) (exact->inexact (evaluator (cadr args))) 1.0))
    (define cur (evaluator place-expr))
    (define num (if (nl-nil? cur) 0.0 (exact->inexact cur)))
    (define new-val (+ num delta))
    (mutate-place! place-expr new-val)
    new-val)
  #t)

;; dec (decrement float place)
(reg-prim! "dec"
  (lambda (evaluator args ctx)
    (define place-expr (car args))
    (define delta (if (pair? (cdr args)) (exact->inexact (evaluator (cadr args))) 1.0))
    (define cur (evaluator place-expr))
    (define num (if (nl-nil? cur) 0.0 (exact->inexact cur)))
    (define new-val (- num delta))
    (mutate-place! place-expr new-val)
    new-val)
  #t)

(reg-prim! "abs"
  (lambda (evaluator args ctx) (abs (car args))))

(reg-prim! "sgn"
  (lambda (evaluator args ctx)
    (define n (car args))
    (cond [(> n 0) 1] [(< n 0) -1] [else 0])))

(reg-prim! "min"
  (lambda (evaluator args ctx)
    (if (pair? (car args))
        (apply min (car args))
        (apply min args))))

(reg-prim! "max"
  (lambda (evaluator args ctx)
    (if (pair? (car args))
        (apply max (car args))
        (apply max args))))

(reg-prim! "pow"
  (lambda (evaluator args ctx) (expt (car args) (cadr args))))

(reg-prim! "sqrt"
  (lambda (evaluator args ctx) (sqrt (car args))))

(reg-prim! "floor"
  (lambda (evaluator args ctx) (floor (car args))))

(reg-prim! "ceil"
  (lambda (evaluator args ctx) (ceiling (car args))))

(reg-prim! "round"
  (lambda (evaluator args ctx)
    (define x (car args))
    (define dec (if (pair? (cdr args)) (cadr args) 0))
    (if (= dec 0)
        (exact-round x)
        (let ([factor (expt 10.0 dec)])
          (/ (round (* x factor)) factor)))))

;; Trig & Transcendental
(reg-prim! "sin" (lambda (e a c) (sin (car a))))
(reg-prim! "cos" (lambda (e a c) (cos (car a))))
(reg-prim! "tan" (lambda (e a c) (tan (car a))))
(reg-prim! "asin" (lambda (e a c) (asin (car a))))
(reg-prim! "acos" (lambda (e a c) (acos (car a))))
(reg-prim! "atan" (lambda (e a c) (atan (car a))))
(reg-prim! "atan2" (lambda (e a c) (atan (car a) (cadr a))))
(reg-prim! "sinh" (lambda (e a c) (sinh (car a))))
(reg-prim! "cosh" (lambda (e a c) (cosh (car a))))
(reg-prim! "tanh" (lambda (e a c) (tanh (car a))))
(define (nl-asinh x) (log (+ x (sqrt (+ (* x x) 1)))))
(define (nl-acosh x) (log (+ x (sqrt (- (* x x) 1)))))
(define (nl-atanh x) (* 0.5 (log (/ (+ 1 x) (- 1 x)))))

(reg-prim! "asinh" (lambda (e a c) (nl-asinh (car a))))
(reg-prim! "acosh" (lambda (e a c) (nl-acosh (car a))))
(reg-prim! "atanh" (lambda (e a c) (nl-atanh (car a))))
(reg-prim! "exp" (lambda (e a c) (exp (car a))))
(reg-prim! "log" (lambda (e a c) (log (car a))))
(reg-prim! "gcd" (lambda (e a c) (apply gcd a)))

(reg-prim! "rand"
  (lambda (e a c)
    (if (pair? a)
        (random (car a))
        (random))))

(reg-prim! "random"
  (lambda (e a c)
    (cond
      [(null? a) (random)]
      [(= (length a) 1) (random (car a))]
      [else (+ (car a) (random (- (cadr a) (car a))))])))

(reg-prim! "seed"
  (lambda (e a c)
    (random-seed (car a))
    (car a)))

(reg-prim! "randomize"
  (lambda (e a c)
    (if (null? a)
        (begin
          (random-seed (modulo (current-milliseconds) 2147483647))
          nl-true)
        (let ([lst (car a)])
          (shuffle lst)))))

;; -------------------------------------------------------------------
;; Bitwise Operators
;; -------------------------------------------------------------------

(reg-prim! "&" (lambda (e a c) (apply bitwise-and a)))
(reg-prim! "|" (lambda (e a c) (apply bitwise-ior a)))
(reg-prim! "^" (lambda (e a c) (apply bitwise-xor a)))
(reg-prim! "~" (lambda (e a c) (bitwise-not (car a))))
(reg-prim! "<<" (lambda (e a c) (arithmetic-shift (car a) (cadr a))))
(reg-prim! ">>" (lambda (e a c) (arithmetic-shift (car a) (- (cadr a)))))

(reg-prim! "bits"
  (lambda (e a c)
    (define n (car a))
    (define width (if (pair? (cdr a)) (cadr a) 32))
    (~r n #:base 2 #:min-width width #:pad-string "0")))

;; -------------------------------------------------------------------
;; Comparisons & Logic
;; -------------------------------------------------------------------

(reg-prim! "="
  (lambda (e a c)
    (racket->nl-bool
     (let loop ([lst a])
       (if (or (null? lst) (null? (cdr lst)))
           #t
           (and (equal? (car lst) (cadr lst))
                (loop (cdr lst))))))))

(reg-prim! "!="
  (lambda (e a c)
    (racket->nl-bool (not (equal? (car a) (cadr a))))))

(define (compare-vals a b)
  (cond
    [(and (number? a) (number? b)) (- a b)]
    [(and (string? a) (string? b)) (cond [(string<? a b) -1] [(string>? a b) 1] [else 0])]
    [(and (nl-symbol? a) (nl-symbol? b))
     (compare-vals (nl-symbol-name a) (nl-symbol-name b))]
    [else
     (compare-vals (nl->string a) (nl->string b))]))

(reg-prim! "<"
  (lambda (e a c)
    (racket->nl-bool
     (let loop ([lst a])
       (if (or (null? lst) (null? (cdr lst)))
           #t
           (and (< (compare-vals (car lst) (cadr lst)) 0)
                (loop (cdr lst))))))))

(reg-prim! ">"
  (lambda (e a c)
    (racket->nl-bool
     (let loop ([lst a])
       (if (or (null? lst) (null? (cdr lst)))
           #t
           (and (> (compare-vals (car lst) (cadr lst)) 0)
                (loop (cdr lst))))))))

(reg-prim! "<="
  (lambda (e a c)
    (racket->nl-bool
     (let loop ([lst a])
       (if (or (null? lst) (null? (cdr lst)))
           #t
           (and (<= (compare-vals (car lst) (cadr lst)) 0)
                (loop (cdr lst))))))))

(reg-prim! ">="
  (lambda (e a c)
    (racket->nl-bool
     (let loop ([lst a])
       (if (or (null? lst) (null? (cdr lst)))
           #t
           (and (>= (compare-vals (car lst) (cadr lst)) 0)
                (loop (cdr lst))))))))

(reg-prim! "not"
  (lambda (e a c)
    (racket->nl-bool (not (nl-truthy? (car a))))))

(reg-prim! "member"
  (lambda (e a c)
    (define item (car a))
    (define target (cadr a))
    (cond
      [(list? target)
       (define res (member item target))
       (if res res nl-nil)]
      [(string? target)
       (define idx (string-contains? target (if (string? item) item (~a item))))
       (racket->nl-bool idx)]
      [else nl-nil])))

;; -------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------

(reg-prim! "atom?" (lambda (e a c) (racket->nl-bool (nl-atom? (car a)))))
(reg-prim! "list?" (lambda (e a c) (racket->nl-bool (nl-list? (car a)))))
(reg-prim! "symbol?" (lambda (e a c) (racket->nl-bool (nl-symbol? (car a)))))
(reg-prim! "string?" (lambda (e a c) (racket->nl-bool (nl-string? (car a)))))
(reg-prim! "number?" (lambda (e a c) (racket->nl-bool (nl-number? (car a)))))
(reg-prim! "integer?" (lambda (e a c) (racket->nl-bool (nl-integer? (car a)))))
(reg-prim! "float?" (lambda (e a c) (racket->nl-bool (nl-float? (car a)))))
(reg-prim! "array?" (lambda (e a c) (racket->nl-bool (nl-array? (car a)))))
(reg-prim! "context?" (lambda (e a c) (racket->nl-bool (nl-context? (car a)))))
(reg-prim! "primitive?" (lambda (e a c) (racket->nl-bool (nl-primitive? (car a)))))
(reg-prim! "lambda?" (lambda (e a c) (racket->nl-bool (and (nl-lambda? (car a)) (not (nl-lambda-is-macro? (car a)))))))
(reg-prim! "macro?" (lambda (e a c) (racket->nl-bool (and (nl-lambda? (car a)) (nl-lambda-is-macro? (car a))))))
(reg-prim! "empty?" (lambda (e a c) (nl-empty? (car a))))
(reg-prim! "nil?" (lambda (e a c) (racket->nl-bool (nl-nil? (car a)))))
(reg-prim! "true?" (lambda (e a c) (racket->nl-bool (nl-true? (car a)))))
(reg-prim! "zero?" (lambda (e a c) (racket->nl-bool (and (number? (car a)) (zero? (car a))))))
(reg-prim! "even?" (lambda (e a c) (racket->nl-bool (and (integer? (car a)) (even? (car a))))))
(reg-prim! "odd?" (lambda (e a c) (racket->nl-bool (and (integer? (car a)) (odd? (car a))))))
(reg-prim! "NaN?" (lambda (e a c) (racket->nl-bool (and (real? (car a)) (nan? (car a))))))
(reg-prim! "inf?" (lambda (e a c) (racket->nl-bool (and (real? (car a)) (infinite? (car a))))))
(reg-prim! "file?" (lambda (e a c) (racket->nl-bool (and (string? (car a)) (file-exists? (car a))))))
(reg-prim! "directory?" (lambda (e a c) (racket->nl-bool (and (string? (car a)) (directory-exists? (car a))))))
(reg-prim! "protected?" (lambda (e a c) (racket->nl-bool (and (nl-symbol? (car a)) (nl-symbol-protected? (car a))))))
(reg-prim! "global?"
  (lambda (e a c)
    (define sym (car a))
    (racket->nl-bool (and (nl-symbol? sym) (string=? (nl-symbol-context-name sym) "MAIN")))))

;; -------------------------------------------------------------------
;; List Operations
;; -------------------------------------------------------------------

(reg-prim! "cons"
  (lambda (e a c)
    (define elem (car a))
    (define lst (cadr a))
    (cond
      [(list? lst) (cons elem lst)]
      [(nl-lambda? lst)
       ;; In newLISP: (cons '(x) (lambda (+ x x))) -> (lambda (x) (+ x x))
       (nl-lambda elem (nl-lambda-body lst) (nl-lambda-is-macro? lst) (nl-lambda-ctx-name lst))]
      [else (list elem lst)])))

(reg-prim! "list"
  (lambda (e a c) a))

(reg-prim! "first"
  (lambda (e a c)
    (define target (car a))
    (cond
      [(pair? target) (car target)]
      [(nl-lambda? target) (nl-lambda-params target)]
      [(nl-array? target) (vector-ref (nl-array-data target) 0)]
      [(string? target) (if (> (string-length target) 0) (substring target 0 1) "")]
      [else nl-nil])))

(reg-prim! "last"
  (lambda (e a c)
    (define target (car a))
    (cond
      [(pair? target) (last target)]
      [(nl-lambda? target) (last (nl-lambda-body target))]
      [(nl-array? target)
       (define v (nl-array-data target))
       (vector-ref v (- (vector-length v) 1))]
      [(string? target)
       (define len (string-length target))
       (if (> len 0) (substring target (- len 1) len) "")]
      [else nl-nil])))

(reg-prim! "rest"
  (lambda (e a c)
    (define target (car a))
    (cond
      [(pair? target) (cdr target)]
      [(nl-lambda? target) (nl-lambda-body target)]
      [(nl-array? target)
       (define l (nl-array->list target))
       (make-nl-array (list (max 0 (- (length l) 1))) (cdr l))]
      [(string? target)
       (if (> (string-length target) 1) (substring target 1) "")]
      [else '()])))

(reg-prim! "nth"
  (lambda (e a c)
    (define idx (car a))
    (define target (cadr a))
    (cond
      [(list? target)
       (nl-index-list target (if (pair? idx) idx (list idx)))]
      [(nl-array? target)
       (nl-array-ref target (if (pair? idx) idx (list idx)))]
      [(string? target)
       (nl-index-string target (if (pair? idx) idx (list idx)))]
      [else nl-nil])))

(reg-prim! "slice"
  (lambda (e a c)
    (define target (car a))
    (define offset (cadr a))
    (define len (if (pair? (cddr a)) (caddr a) #f))
    (cond
      [(list? target) (nl-slice-list target offset len)]
      [(string? target) (nl-slice-string target offset len)]
      [(nl-array? target)
       (define l (nl-array->list target))
       (define sub (nl-slice-list l offset len))
       (make-nl-array (list (length sub)) sub)]
      [else '()])))

(reg-prim! "length"
  (lambda (e a c)
    (define target (car a))
    (cond
      [(list? target) (length target)]
      [(nl-lambda? target) (+ 1 (length (nl-lambda-body target)))]
      [(nl-array? target) (vector-length (nl-array-data target))]
      [(string? target) (string-length target)]
      [else 0])))

(reg-prim! "append"
  (lambda (e a c)
    (cond
      [(null? a) '()]
      [(string? (car a))
       (apply string-append (map (lambda (x) (if (string? x) x (nl->string x #f))) a))]
      [(nl-array? (car a))
       (define flat (apply append (map (lambda (x) (if (nl-array? x) (vector->list (nl-array-data x)) x)) a)))
       (make-nl-array (list (length flat)) flat)]
      [(nl-lambda? (car a))
       (define lam (car a))
       (define to-append (cadr a))
       (nl-lambda (nl-lambda-params lam) (append (nl-lambda-body lam) to-append) (nl-lambda-is-macro? lam) (nl-lambda-ctx-name lam))]
      [else
       (apply append a)])))

(reg-prim! "reverse"
  (lambda (e a c)
    (define target (car a))
    (cond
      [(list? target) (reverse target)]
      [(string? target) (list->string (reverse (string->list target)))]
      [(nl-array? target)
       (define l (reverse (nl-array->list target)))
       (make-nl-array (list (length l)) l)]
      [else target])))

(reg-prim! "flat"
  (lambda (e a c)
    (define (flatten-nl lst)
      (cond
        [(null? lst) '()]
        [(pair? lst) (append (flatten-nl (car lst)) (flatten-nl (cdr lst)))]
        [else (list lst)]))
    (flatten-nl (car a))))

(reg-prim! "chop"
  (lambda (e a c)
    (define target (car a))
    (define n (if (pair? (cdr a)) (cadr a) 1))
    (cond
      [(list? target) (drop-right target (min (length target) n))]
      [(string? target) (substring target 0 (max 0 (- (string-length target) n)))]
      [else target])))

(reg-prim! "count"
  (lambda (e a c)
    (define item (car a))
    (define target (cadr a))
    (cond
      [(list? target) (count (lambda (x) (equal? x item)) target)]
      [(string? target)
       (define s-item (if (string? item) item (string item)))
       (length (regexp-match-positions* (regexp-quote s-item) target))]
      [else 0])))

(reg-prim! "difference"
  (lambda (e a c)
    (define l1 (car a))
    (define l2 (cadr a))
    (filter (lambda (x) (not (member x l2))) l1)))

(reg-prim! "intersect"
  (lambda (e a c)
    (define l1 (car a))
    (define l2 (cadr a))
    (filter (lambda (x) (and (member x l2) #t)) l1)))

(reg-prim! "unique"
  (lambda (e a c)
    (remove-duplicates (car a))))

(reg-prim! "push"
  (lambda (evaluator args ctx)
    (define val (evaluator (car args)))
    (define place-expr (cadr args))
    (define idx (if (pair? (cddr args)) (evaluator (caddr args)) 0))
    (define raw-target (evaluator place-expr))
    (define target (if (nl-nil? raw-target) '() raw-target))
    (cond
      [(list? target)
       (define len (length target))
       (define norm-idx
         (cond
           [(< idx 0) (max 0 (+ len idx 1))]
           [(> idx len) len]
           [else idx]))
       (define-values (head tail) (split-at target norm-idx))
       (define new-list (append head (list val) tail))
       (mutate-place! place-expr new-list)
       new-list]
      [(string? target)
       (define val-str (if (string? val) val (~a val)))
       (define len (string-length target))
       (define norm-idx
         (cond
           [(< idx 0) (max 0 (+ len idx 1))]
           [(> idx len) len]
           [else idx]))
       (define new-str (string-append (substring target 0 norm-idx) val-str (substring target norm-idx)))
       (mutate-place! place-expr new-str)
       new-str]
      [else (error 'push "invalid place for push: ~a" target)]))
  #t)

(reg-prim! "pop"
  (lambda (evaluator args ctx)
    (define place-expr (car args))
    (define idx (if (pair? (cdr args)) (evaluator (cadr args)) 0))
    (define target (evaluator place-expr))
    (cond
      [(list? target)
       (when (null? target) (error 'pop "cannot pop from empty list"))
       (define len (length target))
       (define norm-idx (if (< idx 0) (+ len idx) idx))
       (define popped (list-ref target norm-idx))
       (define new-list (append (take target norm-idx) (drop target (+ norm-idx 1))))
       (mutate-place! place-expr new-list)
       popped]
      [(string? target)
       (define len (string-length target))
       (when (= len 0) (error 'pop "cannot pop from empty string"))
       (define norm-idx (if (< idx 0) (+ len idx) idx))
       (define popped (substring target norm-idx (+ norm-idx 1)))
       (define new-str (string-append (substring target 0 norm-idx) (substring target (+ norm-idx 1))))
       (mutate-place! place-expr new-str)
       popped]
      [else (error 'pop "invalid place for pop: ~a" target)]))
  #t)

(reg-prim! "swap"
  (lambda (evaluator args ctx)
    (define p1 (car args))
    (define p2 (cadr args))
    (define v1 (evaluator p1))
    (define v2 (evaluator p2))
    (mutate-place! p1 v2)
    (mutate-place! p2 v1)
    v2)
  #t)

(reg-prim! "assoc"
  (lambda (e a c)
    (define key (car a))
    (define alist (cadr a))
    (define res (assoc key alist))
    (if res res nl-nil)))

(reg-prim! "lookup"
  (lambda (e a c)
    (define key (car a))
    (define alist (cadr a))
    (define idx (if (pair? (cddr a)) (caddr a) -1))
    (define def-val (if (and (pair? (cddr a)) (pair? (cdddr a))) (cadddr a) nl-nil))
    (define entry (assoc key alist))
    (if entry
        (if (< idx 0)
            (last entry)
            (list-ref entry idx))
        def-val)))

(reg-prim! "ref"
  (lambda (e a c)
    (define key (car a))
    (define target (cadr a))
    (let loop ([cur target] [path '()])
      (cond
        [(equal? cur key) (list (reverse path))]
        [(pair? cur)
         (let sub-loop ([lst cur] [i 0])
           (if (null? lst)
               '()
               (let ([found (loop (car lst) (cons i path))])
                 (if (null? found)
                     (sub-loop (cdr lst) (+ i 1))
                     found))))]
        [else '()]))))

(reg-prim! "sequence"
  (lambda (e a c)
    (define from (car a))
    (define to (cadr a))
    (define step (if (pair? (cddr a)) (caddr a) (if (> to from) 1 -1)))
    (if (> step 0)
        (for/list ([i (in-range from (+ to 1) step)]) i)
        (for/list ([i (in-range from (- to 1) step)]) i))))

(reg-prim! "dup"
  (lambda (e a c)
    (define item (car a))
    (define count (cadr a))
    (cond
      [(string? item)
       (apply string-append (make-list count item))]
      [(list? item)
       (apply append (make-list count item))]
      [else
       (make-list count item)])))

(reg-prim! "explode"
  (lambda (e a c)
    (define target (car a))
    (define chunk (if (pair? (cdr a)) (cadr a) 1))
    (cond
      [(string? target)
       (for/list ([ch (in-string target)]) (string ch))]
      [(list? target)
       (map list target)]
      [else '()])))

(reg-prim! "join"
  (lambda (e a c)
    (define lst (car a))
    (define sep (if (pair? (cdr a)) (cadr a) ""))
    (string-join (map (lambda (x) (if (string? x) x (nl->string x #f))) lst) sep)))

;; Higher-order functions: map, filter, clean, apply
(reg-prim! "apply"
  (lambda (evaluator args ctx)
    (define func (car args))
    (define func-args (cadr args))
    (apply-evaluated func func-args)))

(reg-prim! "map"
  (lambda (evaluator args ctx)
    (define func (car args))
    (define lists (cdr args))
    (apply map (lambda xs (apply-evaluated func xs)) lists)))

(reg-prim! "filter"
  (lambda (evaluator args ctx)
    (define func (car args))
    (define lst (cadr args))
    (filter (lambda (elem)
              (nl-truthy? (apply-evaluated func (list elem))))
            lst)))

(reg-prim! "clean"
  (lambda (evaluator args ctx)
    (define func (car args))
    (define lst (cadr args))
    (filter (lambda (elem)
              (not (nl-truthy? (apply-evaluated func (list elem)))))
            lst)))

(reg-prim! "sort"
  (lambda (evaluator args ctx)
    (define lst (car args))
    (define comp (if (pair? (cdr args)) (cadr args) #f))
    (if comp
        (sort lst (lambda (x y) (nl-truthy? (apply-evaluated comp (list x y)))))
        (sort lst (lambda (x y) (< (compare-vals x y) 0))))))

;; -------------------------------------------------------------------
;; String Operations
;; -------------------------------------------------------------------

(reg-prim! "string"
  (lambda (e a c)
    (apply string-append (map (lambda (x) (nl->string x #f)) a))))

(reg-prim! "char"
  (lambda (e a c)
    (define arg (car a))
    (if (number? arg)
        (string (integer->char arg))
        (char->integer (string-ref (if (string? arg) arg (~a arg)) 0)))))

(reg-prim! "upper-case"
  (lambda (e a c) (string-upcase (car a))))

(reg-prim! "lower-case"
  (lambda (e a c) (string-downcase (car a))))

(reg-prim! "title-case"
  (lambda (e a c) (string-titlecase (car a))))

(reg-prim! "trim"
  (lambda (e a c) (string-trim (car a))))

(reg-prim! "starts-with"
  (lambda (e a c)
    (define str (car a))
    (define prefix (cadr a))
    (racket->nl-bool (string-prefix? str prefix))))

(reg-prim! "ends-with"
  (lambda (e a c)
    (define str (car a))
    (define suffix (cadr a))
    (racket->nl-bool (string-suffix? str suffix))))

(reg-prim! "search"
  (lambda (e a c)
    (define pat (car a))
    (define str (cadr a))
    (define res (string-contains? str pat))
    (if res (string-contains? str pat) nl-nil)))

;; Format: (format str-format exp1 exp2 ...)
(reg-prim! "format"
  (lambda (e a c)
    (define fmt (car a))
    (define data (cdr a))
    ;; Simple C printf formatter for newLISP
    (nl-printf fmt data)))

(define (nl-printf fmt args)
  (define out (open-output-string))
  (define in (open-input-string fmt))
  (define cur-args args)
  (let loop ()
    (define c (read-char in))
    (cond
      [(eof-object? c) (get-output-string out)]
      [(char=? c #\%)
       (define next-c (read-char in))
       (cond
         [(eof-object? next-c) (write-char #\% out)]
         [(char=? next-c #\%) (write-char #\% out)]
         [(char=? next-c #\s)
          (when (pair? cur-args)
            (display (nl->string (car cur-args) #f) out)
            (set! cur-args (cdr cur-args)))
          (loop)]
         [(or (char=? next-c #\d) (char=? next-c #\i))
          (when (pair? cur-args)
            (display (inexact->exact (truncate (car cur-args))) out)
            (set! cur-args (cdr cur-args)))
          (loop)]
         [(char=? next-c #\f)
          (when (pair? cur-args)
            (display (~r (exact->inexact (car cur-args)) #:precision '(= 6)) out)
            (set! cur-args (cdr cur-args)))
          (loop)]
         [(char=? next-c #\x)
          (when (pair? cur-args)
            (display (~r (inexact->exact (car cur-args)) #:base 16) out)
            (set! cur-args (cdr cur-args)))
          (loop)]
         [(char=? next-c #\X)
          (when (pair? cur-args)
            (display (string-upcase (~r (inexact->exact (car cur-args)) #:base 16)) out)
            (set! cur-args (cdr cur-args)))
          (loop)]
         [(char=? next-c #\c)
          (when (pair? cur-args)
            (write-char (integer->char (car cur-args)) out)
            (set! cur-args (cdr cur-args)))
          (loop)]
         [else
          (write-char #\% out)
          (write-char next-c out)
          (loop)])]
      [else
       (write-char c out)
       (loop)])))

;; Regex search: (regex pattern str [opt [offset]])
(reg-prim! "regex"
  (lambda (e a c)
    (define pat-str (car a))
    (define target (cadr a))
    (define rx (pregexp pat-str))
    (define m (regexp-match-positions rx target))
    (if m
        (let ([full-match (substring target (caar m) (cdar m))]
              [captures (map (lambda (p) (if p (substring target (car p) (cdr p)) nl-nil)) (cdr m))])
          (set-symbol-val! (find-or-create-symbol "$0" main-context) full-match)
          (for ([cap captures] [i (in-naturals 1)])
            (define sym (find-or-create-symbol (format "$~a" i) main-context))
            (set-symbol-val! sym cap))
          (cons full-match captures))
        nl-nil)))

;; Replace in string: (replace pattern str replacement [opt])
(reg-prim! "replace"
  (lambda (e a c)
    (define pat (car a))
    (define str (cadr a))
    (define rep (caddr a))
    (if (string? pat)
        (regexp-replace* (pregexp (regexp-quote pat)) str rep)
        (regexp-replace* pat str rep))))

;; Parse string: (parse str [break-str])
(reg-prim! "parse"
  (lambda (e a c)
    (define str (car a))
    (if (pair? (cdr a))
        (string-split str (cadr a) #:trim? #f)
        ;; Default newLISP parser tokens
        (map (lambda (x) (nl->string x #f)) (nl-read-all str)))))

;; -------------------------------------------------------------------
;; Arrays
;; -------------------------------------------------------------------

(reg-prim! "array"
  (lambda (e a c)
    (define-values (dims init)
      (let split-args ([rem a] [accum-dims '()])
        (cond
          [(null? rem) (values (reverse accum-dims) '())]
          [(and (= (length rem) 1) (list? (car rem)))
           (values (reverse accum-dims) (car rem))]
          [(exact-integer? (car rem))
           (split-args (cdr rem) (cons (car rem) accum-dims))]
          [else (values (reverse accum-dims) rem)])))
    (make-nl-array dims init)))

(reg-prim! "array-list"
  (lambda (e a c)
    (nl-array->list (car a))))

(reg-prim! "transpose"
  (lambda (e a c)
    (define mat (if (nl-array? (car a)) (nl-array->list (car a)) (car a)))
    (apply map list mat)))

;; -------------------------------------------------------------------
;; Context, FOOP, Reflection, & Systems
;; -------------------------------------------------------------------

(reg-prim! "context"
  (lambda (e a c)
    (if (null? a)
        (current-context)
        (let ([arg (car a)])
          (define ctx
            (cond
              [(nl-context? arg) arg]
              [(nl-symbol? arg) (get-or-create-context (nl-symbol-name arg))]
              [(string? arg) (get-or-create-context arg)]
              [else (error 'context "invalid context designator: ~a" arg)]))
          (current-context ctx)
          ctx))))

(reg-prim! "current-context"
  (lambda (e a c) (current-context)))

(reg-prim! "symbols"
  (lambda (e a c)
    (define target-ctx (if (pair? a)
                           (if (nl-context? (car a)) (car a) (get-or-create-context (nl-symbol-name (car a))))
                           (current-context)))
    (hash-values (nl-context-symbols target-ctx))))

(reg-prim! "sym"
  (lambda (e a c)
    (define name (car a))
    (define target-ctx (if (pair? (cdr a))
                           (if (nl-context? (cadr a)) (cadr a) (get-or-create-context (nl-symbol-name (cadr a))))
                           (current-context)))
    (find-or-create-symbol (if (string? name) name (~a name)) target-ctx)))

(reg-prim! "new"
  (lambda (e a c)
    (define src
      (cond
        [(nl-context? (car a)) (car a)]
        [(nl-symbol? (car a))
         (define v (nl-symbol-value (car a)))
         (if (nl-context? v) v (get-or-create-context (nl-symbol-name (car a))))]
        [else (get-or-create-context (~a (car a)))]))
    (define target-sym (if (pair? (cdr a)) (cadr a) #f))
    (define target-name
      (cond
        [(nl-symbol? target-sym) (nl-symbol-name target-sym)]
        [(string? target-sym) target-sym]
        [else (~a target-sym)]))
    (define target-ctx
      (if target-sym
          (get-or-create-context target-name)
          (current-context)))
    ;; Copy symbols from source context
    (for ([(k sym) (in-hash (nl-context-symbols src))])
      (define target-k (if (string=? k (nl-context-name src))
                           (nl-context-name target-ctx)
                           k))
      (define new-sym (find-or-create-symbol target-k target-ctx))
      (set-nl-symbol-value! new-sym (nl-symbol-value sym)))
    (when (nl-context-default-functor src)
      (define def-func
        (let ([def-sym (hash-ref (nl-context-symbols target-ctx) (nl-context-name target-ctx) #f)])
          (if (and def-sym (not (nl-nil? (nl-symbol-value def-sym))))
              (nl-symbol-value def-sym)
              (nl-context-default-functor src))))
      (set-nl-context-default-functor! target-ctx def-func))
    target-ctx))

(reg-prim! "self"
  (lambda (e a c)
    (define target (current-self-target))
    (if (null? a)
        target
        (nl-index-list target a))))

(reg-prim! "args"
  (lambda (e a c)
    (define extra (current-call-args))
    (if (null? a)
        extra
        (nl-index-list extra a))))

(reg-prim! "main-args"
  (lambda (e a c)
    (nl-symbol-value sym-main-args)))

(reg-prim! "env"
  (lambda (e a c)
    (if (null? a)
        (for/list ([entry (environment-variables-names (current-environment-variables))])
          (list (bytes->string/utf-8 entry) (getenv (bytes->string/utf-8 entry))))
        (let ([val (getenv (car a))])
          (if val val nl-nil)))))

(reg-prim! "ostype"
  (lambda (e a c)
    (case (system-type 'os)
      [(windows) "Win32"]
      [(macosx) "OSX"]
      [else "Linux"])))

(reg-prim! "date"
  (lambda (e a c)
    (define secs (if (pair? a) (car a) (current-seconds)))
    (date->string (seconds->date secs) #t)))

(reg-prim! "now"
  (lambda (e a c)
    (define d (current-date))
    (list (date-year d) (date-month d) (date-day d)
          (date-hour d) (date-minute d) (date-second d)
          0 0 (date-week-day d) 0)))

(reg-prim! "time"
  (lambda (evaluator args ctx)
    (define expr (car args))
    (define n (if (pair? (cdr args)) (evaluator (cadr args)) 1))
    (define t0 (current-inexact-milliseconds))
    (for ([_ n]) (evaluator expr))
    (define t1 (current-inexact-milliseconds))
    (/ (- t1 t0) (exact->inexact n)))
  #t)

(reg-prim! "time-of-day"
  (lambda (e a c) (current-inexact-milliseconds)))

(reg-prim! "sleep"
  (lambda (e a c)
    (sleep (/ (car a) 1000.0))
    nl-true))

(reg-prim! "eval"
  (lambda (e a c)
    (nl-eval (car a))))

(reg-prim! "eval-string"
  (lambda (e a c)
    (define str (car a))
    (define ctx-target (if (pair? (cdr a)) (cadr a) #f))
    (define exprs (nl-read-all str (lambda (s) (find-or-create-symbol s (or ctx-target (current-context))))))
    (eval-body exprs)))

(reg-prim! "read-expr"
  (lambda (e a c)
    (define str (car a))
    (define ctx-target (if (pair? (cdr a)) (cadr a) #f))
    (nl-read-expr str (lambda (s) (find-or-create-symbol s (or ctx-target (current-context)))))))

;; -------------------------------------------------------------------
;; HTTP Networking API (get-url, post-url, put-url, delete-url)
;; -------------------------------------------------------------------

(reg-prim! "get-url" (lambda (e a c) (nl-get-url a)))
(reg-prim! "post-url" (lambda (e a c) (nl-post-url a)))
(reg-prim! "put-url" (lambda (e a c) (nl-put-url a)))
(reg-prim! "delete-url" (lambda (e a c) (nl-delete-url a)))
(reg-prim! "base64-enc" (lambda (e a c) (nl-base64-enc a)))
(reg-prim! "base64-dec" (lambda (e a c) (nl-base64-dec a)))
(reg-prim! "json-error" (lambda (e a c) (nl-json-error)))
(reg-prim! "xml-parse" (lambda (e a c) (nl-xml-parse a)))
(reg-prim! "xml-error" (lambda (e a c) (nl-xml-error)))
(reg-prim! "xml-type-tags" (lambda (e a c) (nl-xml-type-tags a)))
(reg-prim! "xfer-event" (lambda (e a c) (nl-xfer-event a)))

;; -------------------------------------------------------------------
;; File I/O & Process Management
;; -------------------------------------------------------------------

(reg-prim! "print"
  (lambda (e a c)
    (for ([item a])
      (display (if (string? item) item (nl->string item #f))))
    (if (pair? a) (last a) nl-nil)))

(reg-prim! "println"
  (lambda (e a c)
    (for ([item a])
      (display (if (string? item) item (nl->string item #f))))
    (newline)
    (if (pair? a) (last a) nl-nil)))

(reg-prim! "read-file"
  (lambda (e a c)
    (define path (car a))
    (if (file-exists? path)
        (file->string path)
        nl-nil)))

(reg-prim! "write-file"
  (lambda (e a c)
    (define path (car a))
    (define content (cadr a))
    (display-to-file (if (string? content) content (nl->string content #f)) path #:exists 'replace)
    (string-length (if (string? content) content (nl->string content #f)))))

(reg-prim! "append-file"
  (lambda (e a c)
    (define path (car a))
    (define content (cadr a))
    (display-to-file (if (string? content) content (nl->string content #f)) path #:exists 'append)
    (string-length (if (string? content) content (nl->string content #f)))))

(reg-prim! "file-info"
  (lambda (e a c)
    (define path (car a))
    (if (file-exists? path)
        (list (file-size path)
              0 0
              (file-or-directory-modify-seconds path))
        nl-nil)))

(reg-prim! "copy-file"
  (lambda (e a c)
    (copy-file (car a) (cadr a) #t)
    nl-true))

(reg-prim! "delete-file"
  (lambda (e a c)
    (if (file-exists? (car a))
        (begin (delete-file (car a)) nl-true)
        nl-nil)))

(reg-prim! "rename-file"
  (lambda (e a c)
    (rename-file-or-directory (car a) (cadr a) #t)
    nl-true))

(reg-prim! "make-dir"
  (lambda (e a c)
    (make-directory* (car a))
    nl-true))

(reg-prim! "directory"
  (lambda (e a c)
    (define path (if (pair? a) (car a) "."))
    (if (directory-exists? path)
        (map path->string (directory-list path))
        nl-nil)))

(reg-prim! "change-dir"
  (lambda (e a c)
    (current-directory (car a))
    (path->string (current-directory))))

(reg-prim! "real-path"
  (lambda (e a c)
    (path->string (simplify-path (path->complete-path (car a))))))

(reg-prim! "load"
  (lambda (e a c)
    (define filename (car a))
    (if (file-exists? filename)
        (let* ([source (file->string filename)]
               [exprs (nl-read-all source (lambda (s) (find-or-create-symbol s (current-context))))])
          (eval-body exprs))
        (error 'load "file not found: ~a" filename))))

(reg-prim! "exec"
  (lambda (e a c)
    (define cmd (car a))
    (define out (open-output-string))
    (parameterize ([current-output-port out])
      (system cmd))
    (string-split (get-output-string out) "\n")))

(reg-prim! "exit"
  (lambda (e a c)
    (exit (if (pair? a) (car a) 0))))

;; -------------------------------------------------------------------
;; JSON & XML
;; -------------------------------------------------------------------

(reg-prim! "json-parse"
  (lambda (e a c)
    (define str (car a))
    (define (racket-json->nl j)
      (cond
        [(hash? j)
         (for/list ([(k v) (in-hash j)])
           (list (symbol->string k) (racket-json->nl v)))]
        [(list? j) (map racket-json->nl j)]
        [(eq? j #t) nl-true]
        [(eq? j #f) nl-nil]
        [else j]))
    (racket-json->nl (string->jsexpr str))))

;; Initialize Class:Class constructor
;; (define (Class:Class) (cons (context) (args)))
(define class-sym (find-or-create-symbol "Class" class-context))
(define class-ctor
  (nl-primitive "Class:Class"
    (lambda (e a c)
      (cons (current-context) (if (pair? a) a (current-call-args))))
    #f 0 #f "19ab01fe0dcd6b5"))
(set-symbol-val! class-sym class-ctor)
(set-nl-context-default-functor! class-context class-ctor)

;; Ensure symbol Class in MAIN points to class-context
(set-nl-symbol-value! (find-or-create-symbol "Class" main-context) class-context)

;; -------------------------------------------------------------------
;; Registration of Extended Manual Primitives
;; -------------------------------------------------------------------

;; File I/O & Streams
(reg-prim! "open" (lambda (e a c) (nl-open a)))
(reg-prim! "close" (lambda (e a c) (nl-close a)))
(reg-prim! "read" (lambda (e a c) (nl-file-read a)) #t)
(reg-prim! "write" (lambda (e a c) (nl-write a)))
(reg-prim! "read-line" (lambda (e a c) (nl-read-line a)))
(reg-prim! "write-line" (lambda (e a c) (nl-write-line a)))
(reg-prim! "read-char" (lambda (e a c) (nl-read-char a)))
(reg-prim! "write-char" (lambda (e a c) (nl-write-char a)))
(reg-prim! "read-utf8" (lambda (e a c) (nl-read-utf8 a)))
(reg-prim! "read-key" (lambda (e a c) (nl-read-key)))
(reg-prim! "seek" (lambda (e a c) (nl-seek a)))
(reg-prim! "peek" (lambda (e a c) (nl-peek a)))
(reg-prim! "device" (lambda (e a c) (nl-device a)))
(reg-prim! "current-line" (lambda (e a c) (nl-current-line)))
(reg-prim! "save" (lambda (e a c) (nl-save a)))
(reg-prim! "remove-dir" (lambda (e a c) (nl-remove-dir a)))

;; Matrix Operations
(reg-prim! "mat" (lambda (e a c) (nl-mat a)))
(reg-prim! "det" (lambda (e a c) (nl-det a)))
(reg-prim! "invert" (lambda (e a c) (nl-invert a)))
(reg-prim! "multiply" (lambda (e a c) (nl-multiply a)))

;; Math, Financial & Statistics
(reg-prim! "factor" (lambda (e a c) (nl-factor a)))
(reg-prim! "binomial" (lambda (e a c) (nl-binomial a)))
(reg-prim! "erf" (lambda (e a c) (nl-erf a)))
(reg-prim! "beta" (lambda (e a c) (nl-beta a)))
(reg-prim! "betai" (lambda (e a c) (nl-betai a)))
(reg-prim! "gammaln" (lambda (e a c) (nl-gammaln a)))
(reg-prim! "gammai" (lambda (e a c) (nl-gammai a)))
(reg-prim! "series" (lambda (e a c) (nl-series a)))
(reg-prim! "ssq" (lambda (e a c) (nl-ssq a)))
(reg-prim! "stats" (lambda (e a c) (nl-stats a)))
(reg-prim! "corr" (lambda (e a c) (nl-corr a)))
(reg-prim! "normal" (lambda (e a c) (nl-normal a)))
(reg-prim! "t-test" (lambda (e a c) (nl-t-test a)))
(reg-prim! "crit-z" (lambda (e a c) (nl-crit-z a)))
(reg-prim! "crit-t" (lambda (e a c) (nl-crit-t a)))
(reg-prim! "crit-chi2" (lambda (e a c) (nl-crit-chi2 a)))
(reg-prim! "crit-f" (lambda (e a c) (nl-crit-f a)))
(reg-prim! "prob-z" (lambda (e a c) (nl-prob-z a)))
(reg-prim! "prob-t" (lambda (e a c) (nl-prob-t a)))
(reg-prim! "prob-chi2" (lambda (e a c) (nl-prob-chi2 a)))
(reg-prim! "prob-f" (lambda (e a c) (nl-prob-f a)))
(reg-prim! "pv" (lambda (e a c) (nl-pv a)))
(reg-prim! "fv" (lambda (e a c) (nl-fv a)))
(reg-prim! "nper" (lambda (e a c) (nl-nper a)))
(reg-prim! "pmt" (lambda (e a c) (nl-pmt a)))
(reg-prim! "npv" (lambda (e a c) (nl-npv a)))
(reg-prim! "irr" (lambda (e a c) (nl-irr a)))

;; Socket Networking
(reg-prim! "net-listen" (lambda (e a c) (nl-net-listen a)))
(reg-prim! "net-connect" (lambda (e a c) (nl-net-connect a)))
(reg-prim! "net-accept" (lambda (e a c) (nl-net-accept a)))
(reg-prim! "net-close" (lambda (e a c) (nl-net-close a)))
(reg-prim! "net-send" (lambda (e a c) (nl-net-send a)))
(reg-prim! "net-receive" (lambda (e a c) (nl-net-receive a)) #t)
(reg-prim! "net-peek" (lambda (e a c) (nl-net-peek a)))
(reg-prim! "net-select" (lambda (e a c) (nl-net-select a)))
(reg-prim! "net-local" (lambda (e a c) (nl-net-local a)))
(reg-prim! "net-peer" (lambda (e a c) (nl-net-peer a)))
(reg-prim! "net-lookup" (lambda (e a c) (nl-net-lookup a)))
(reg-prim! "net-ping" (lambda (e a c) (nl-net-ping a)))
(reg-prim! "net-interface" (lambda (e a c) (nl-net-interface)))
(reg-prim! "net-error" (lambda (e a c) (nl-net-error)))
(reg-prim! "net-sessions" (lambda (e a c) (nl-net-sessions)))
(reg-prim! "net-service" (lambda (e a c) (nl-net-service a)))
(reg-prim! "net-ipv" (lambda (e a c) (nl-net-ipv a)))
(reg-prim! "net-eval" (lambda (e a c) (nl-net-eval a)))
(reg-prim! "net-send-to" (lambda (e a c) (nl-net-send-to a)))
(reg-prim! "net-receive-from" (lambda (e a c) (nl-net-receive-from a)) #t)
(reg-prim! "net-send-udp" (lambda (e a c) (nl-net-send-to a)))
(reg-prim! "net-receive-udp" (lambda (e a c) (nl-net-receive-from a)) #t)
(reg-prim! "net-packet" (lambda (e a c) (nl-net-packet a)))

;; Predicates
(reg-prim! "null?" (lambda (e a c) (nl-null? a)))
(reg-prim! "quote?" (lambda (e a c) (nl-quote? a)))
(reg-prim! "legal?" (lambda (e a c) (nl-legal? a)))
(reg-prim! "bigint?" (lambda (e a c) (nl-bigint? a)))

;; Conversions & Strings
(reg-prim! "int" (lambda (e a c) (nl-int a)))
(reg-prim! "float" (lambda (e a c) (nl-float a)))
(reg-prim! "flt" (lambda (e a c) (nl-float a)))
(reg-prim! "bigint" (lambda (e a c) (nl-bigint a)))
(reg-prim! "name" (lambda (e a c) (nl-name a)))
(reg-prim! "prefix" (lambda (e a c) (nl-prefix a)))
(reg-prim! "address" (lambda (e a c) (nl-address a)))
(reg-prim! "unicode" (lambda (e a c) (nl-unicode a)))
(reg-prim! "utf8" (lambda (e a c) (nl-utf8 a)))
(reg-prim! "utf8len" (lambda (e a c) (nl-utf8len a)))
(reg-prim! "crc32" (lambda (e a c) (nl-crc32 a)))
(reg-prim! "encrypt" (lambda (e a c) (nl-encrypt a)))
(reg-prim! "regex-comp" (lambda (e a c) (nl-regex-comp a)))
(reg-prim! "pack" (lambda (e a c) (nl-pack a)))
(reg-prim! "unpack" (lambda (e a c) (nl-unpack a)))
(reg-prim! "struct" (lambda (e a c) (nl-struct a)))
(reg-prim! "get-char" (lambda (e a c) (nl-get-char a)))
(reg-prim! "get-int" (lambda (e a c) (nl-get-int a)))
(reg-prim! "get-long" (lambda (e a c) (nl-get-long a)))
(reg-prim! "get-float" (lambda (e a c) (nl-get-float a)))
(reg-prim! "get-string" (lambda (e a c) (nl-get-string a)))

;; List Processing & Pattern Matching
(reg-prim! "exists" (lambda (e a c) (nl-exists a)))
(reg-prim! "for-all" (lambda (e a c) (nl-for-all a)))
(reg-prim! "index" (lambda (e a c) (nl-index a)))
(reg-prim! "select" (lambda (e a c) (nl-select a)))
(reg-prim! "rotate" (lambda (e a c) (nl-rotate a)))
(reg-prim! "extend" (lambda (e a c) (nl-extend a)))
(reg-prim! "set-ref" (lambda (e a c) (nl-set-ref a)))
(reg-prim! "set-ref-all" (lambda (e a c) (nl-set-ref-all a)))
(reg-prim! "pop-assoc" (lambda (e a c) (nl-pop-assoc a)) #t)
(reg-prim! "union" (lambda (e a c) (nl-union a)))
(reg-prim! "match" (lambda (e a c) (nl-match a)))
(reg-prim! "unify" (lambda (e a c) (nl-unify a)))

;; Date and Time
(reg-prim! "date-list" (lambda (e a c) (nl-date-list a)))
(reg-prim! "date-value" (lambda (e a c) (nl-date-value a)))
(reg-prim! "date-parse" (lambda (e a c) (nl-date-parse a)))

;; System, Process & Events
(reg-prim! "!" (lambda (e a c) (nl-shell-exec a)))
(reg-prim! "$" (lambda (e a c) (nl-regex-dollar a)))
(reg-prim! "delete" (lambda (e a c) (nl-delete a)))
(reg-prim! "reset" (lambda (e a c) (nl-reset)))
(reg-prim! "sys-info" (lambda (e a c) (nl-sys-info)))
(reg-prim! "sys-error" (lambda (e a c) (nl-sys-error a)))
(reg-prim! "last-error" (lambda (e a c) (nl-last-error)))
(reg-prim! "uuid" (lambda (e a c) (nl-uuid)))
(reg-prim! "timer" (lambda (e a c) (nl-timer a)))
(reg-prim! "pretty-print" (lambda (e a c) (nl-pretty-print a)))
(reg-prim! "term" (lambda (e a c) (nl-term)))
(reg-prim! "set-locale" (lambda (e a c) (nl-set-locale a)))
(reg-prim! "source" (lambda (e a c) (nl-source a)))
(reg-prim! "error-event" (lambda (e a c) (nl-error-event a)))
(reg-prim! "command-event" (lambda (e a c) (nl-command-event a)))
(reg-prim! "prompt-event" (lambda (e a c) (nl-prompt-event a)))
(reg-prim! "reader-event" (lambda (e a c) (nl-reader-event a)))

;; Multiprocessing / Cilk & Browser Stubs
(reg-prim! "fork" (lambda (e a c) (nl-fork a)))
(reg-prim! "process" (lambda (e a c) (nl-process a)))
(reg-prim! "wait-pid" (lambda (e a c) (nl-wait-pid a)))
(reg-prim! "abort" (lambda (e a c) (nl-abort a)))
(reg-prim! "destroy" (lambda (e a c) (nl-destroy a)))
(reg-prim! "spawn" (lambda (e a c) (nl-spawn a)))
(reg-prim! "sync" (lambda (e a c) (nl-sync a)))
(reg-prim! "send" (lambda (e a c) (nl-send a)))
(reg-prim! "receive" (lambda (e a c) (nl-receive a)))
(reg-prim! "share" (lambda (e a c) (nl-share a)))
(reg-prim! "semaphore" (lambda (e a c) (nl-semaphore a)))
(reg-prim! "display-html" (lambda (e a c) (nl-display-html a)))
(reg-prim! "eval-string-js" (lambda (e a c) (nl-eval-string-js a)))
(reg-prim! "import" (lambda (e a c) (nl-import a)))
(reg-prim! "dump" (lambda (e a c) (nl-dump a)))
(reg-prim! "cpymem" (lambda (e a c) (nl-cpymem a)))
(reg-prim! "history" (lambda (e a c) (nl-history)))
(reg-prim! "signal" (lambda (e a c) (nl-signal a)))
(reg-prim! "debug" (lambda (e a c) (nl-debug a)))
(reg-prim! "trace" (lambda (e a c) (nl-trace a)))
(reg-prim! "trace-highlight" (lambda (e a c) (nl-trace-highlight a)))
(reg-prim! "throw-error" (lambda (e a c) (nl-throw-error a)))
(reg-prim! "copy" (lambda (e a c) (nl-copy a)))
(reg-prim! "amb" (lambda (e a c) (nl-amb a)))
(reg-prim! "bayes-train" (lambda (e a c) (nl-bayes-train a)))
(reg-prim! "bayes-query" (lambda (e a c) (nl-bayes-query a)))
(reg-prim! "kmeans-train" (lambda (e a c) (nl-kmeans-train a)))
(reg-prim! "kmeans-query" (lambda (e a c) (nl-kmeans-query a)))
(reg-prim! "fft" (lambda (e a c) (nl-fft a)))
(reg-prim! "ifft" (lambda (e a c) (nl-ifft a)))
(reg-prim! "find" (lambda (e a c) (nl-find a)))
(reg-prim! "find-all" (lambda (e a c) (nl-find-all a)))
(reg-prim! "ref-all" (lambda (e a c) (nl-ref-all a)))
(reg-prim! "callback" (lambda (e a c) (nl-callback a)))
(reg-prim! "pipe" (lambda (e a c) (nl-pipe)))

