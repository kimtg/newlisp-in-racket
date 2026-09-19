#lang racket/base

(require racket/math
         racket/list
         racket/string
         racket/format
         "nl-types.rkt"
         "nl-eval.rkt")

(provide (all-defined-out))

;; -------------------------------------------------------------------
;; Fast Math Operations
;; -------------------------------------------------------------------

(define-syntax-rule (nl-fast-+ a b) (+ a b))
(define-syntax-rule (nl-fast-- a b) (- a b))
(define-syntax-rule (nl-fast-* a b) (* a b))
(define-syntax-rule (nl-fast-/ a b) (/ a b))
(define-syntax-rule (nl-fast-% a b) (modulo a b))

(define (nl-nary-+ . args) (if (null? args) 0 (apply + args)))
(define (nl-nary-- . args) (cond [(null? args) 0] [(= (length args) 1) (- (car args))] [else (apply - args)]))
(define (nl-nary-* . args) (if (null? args) 1 (apply * args)))
(define (nl-nary-/ . args) (cond [(null? args) 1] [(= (length args) 1) (/ 1 (car args))] [else (apply / args)]))

;; -------------------------------------------------------------------
;; Fast Comparisons (return nl-true or nl-nil)
;; -------------------------------------------------------------------

(define-syntax-rule (nl-fast-< a b) (if (< a b) nl-true nl-nil))
(define-syntax-rule (nl-fast-> a b) (if (> a b) nl-true nl-nil))
(define-syntax-rule (nl-fast-<= a b) (if (<= a b) nl-true nl-nil))
(define-syntax-rule (nl-fast->= a b) (if (>= a b) nl-true nl-nil))
(define-syntax-rule (nl-fast-= a b) (if (equal? a b) nl-true nl-nil))
(define-syntax-rule (nl-fast-!= a b) (if (not (equal? a b)) nl-true nl-nil))

;; -------------------------------------------------------------------
;; Conditionals with $it update
;; -------------------------------------------------------------------

(define-syntax nl-if
  (syntax-rules ()
    [(_ c t)
     (let ([it c])
       (set-nl-symbol-value! sym-it it)
       (if (nl-truthy? it) t nl-nil))]
    [(_ c t e)
     (let ([it c])
       (set-nl-symbol-value! sym-it it)
       (if (nl-truthy? it) t e))]
    [(_ c1 t1 c2 t2 rest ...)
     (let ([it c1])
       (set-nl-symbol-value! sym-it it)
       (if (nl-truthy? it)
           t1
           (nl-if c2 t2 rest ...)))]))

(define-syntax nl-if-not
  (syntax-rules ()
    [(_ c t)
     (let ([it c])
       (set-nl-symbol-value! sym-it it)
       (if (not (nl-truthy? it)) t nl-nil))]
    [(_ c t e)
     (let ([it c])
       (set-nl-symbol-value! sym-it it)
       (if (not (nl-truthy? it)) t e))]
    [(_ c1 t1 c2 t2 rest ...)
     (let ([it c1])
       (set-nl-symbol-value! sym-it it)
       (if (not (nl-truthy? it))
           t1
           (nl-if-not c2 t2 rest ...)))]))

(define-syntax-rule (nl-when c body ...)
  (let ([it c])
    (set-nl-symbol-value! sym-it it)
    (if (nl-truthy? it)
        (begin body ...)
        nl-nil)))

(define-syntax-rule (nl-unless c body ...)
  (let ([it c])
    (set-nl-symbol-value! sym-it it)
    (if (not (nl-truthy? it))
        (begin body ...)
        nl-nil)))

(define-syntax nl-cond
  (syntax-rules ()
    [(_) nl-nil]
    [(_ (c body ...) rest ...)
     (let ([it c])
       (set-nl-symbol-value! sym-it it)
       (if (nl-truthy? it)
           (begin body ...)
           (nl-cond rest ...)))]))

(define-syntax nl-case
  (syntax-rules ()
    [(_ val-expr) nl-nil]
    [(_ val-expr (key body ...) rest ...)
     (let ([v val-expr])
       (if (if (pair? 'key)
               (member v 'key)
               (equal? v 'key))
           (begin body ...)
           (nl-case v rest ...)))]))

;; -------------------------------------------------------------------
;; Loops with $idx and $it update
;; -------------------------------------------------------------------

(define-syntax-rule (nl-while test-cond body ...)
  (let loop ([idx 0] [last-val nl-nil])
    (define it test-cond)
    (set-nl-symbol-value! sym-it it)
    (if (nl-truthy? it)
        (begin
          (set-nl-symbol-value! sym-idx idx)
          (let ([res (begin body ...)])
            (loop (+ idx 1) res)))
        last-val)))

(define-syntax-rule (nl-until test-cond body ...)
  (let loop ([idx 0] [last-val nl-nil])
    (define it test-cond)
    (set-nl-symbol-value! sym-it it)
    (if (not (nl-truthy? it))
        (begin
          (set-nl-symbol-value! sym-idx idx)
          (let ([res (begin body ...)])
            (loop (+ idx 1) res)))
        last-val)))

(define-syntax-rule (nl-do-while test-cond body ...)
  (let loop ([idx 0] [last-val nl-nil])
    (set-nl-symbol-value! sym-idx idx)
    (let ([res (begin body ...)])
      (define it test-cond)
      (set-nl-symbol-value! sym-it it)
      (if (nl-truthy? it)
          (loop (+ idx 1) res)
          res))))

(define-syntax-rule (nl-do-until test-cond body ...)
  (let loop ([idx 0] [last-val nl-nil])
    (set-nl-symbol-value! sym-idx idx)
    (let ([res (begin body ...)])
      (define it test-cond)
      (set-nl-symbol-value! sym-it it)
      (if (not (nl-truthy? it))
          (loop (+ idx 1) res)
          res))))

(define-syntax nl-dotimes
  (syntax-rules ()
    [(_ (var count-expr) body ...)
     (let ([limit count-expr]
           [saved-var (nl-symbol-value var)]
           [saved-idx (nl-symbol-value sym-idx)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([i 0] [last-val nl-nil])
             (if (< i limit)
                 (begin
                   (set-nl-symbol-value! var i)
                   (set-nl-symbol-value! sym-idx i)
                   (let ([res (begin body ...)])
                     (loop (+ i 1) res)))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var)
           (set-nl-symbol-value! sym-idx saved-idx))))]
    [(_ (var count-expr break-cond) body ...)
     (let ([limit count-expr]
           [saved-var (nl-symbol-value var)]
           [saved-idx (nl-symbol-value sym-idx)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([i 0] [last-val nl-nil])
             (if (< i limit)
                 (begin
                   (set-nl-symbol-value! var i)
                   (set-nl-symbol-value! sym-idx i)
                   (if (nl-truthy? break-cond)
                       last-val
                       (let ([res (begin body ...)])
                         (loop (+ i 1) res))))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var)
           (set-nl-symbol-value! sym-idx saved-idx))))]))

(define-syntax nl-dolist
  (syntax-rules ()
    [(_ (var list-expr) body ...)
     (let ([items list-expr]
           [saved-var (nl-symbol-value var)]
           [saved-idx (nl-symbol-value sym-idx)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([cur items] [i 0] [last-val nl-nil])
             (if (and (pair? cur) (not (null? cur)))
                 (begin
                   (set-nl-symbol-value! var (car cur))
                   (set-nl-symbol-value! sym-idx i)
                   (let ([res (begin body ...)])
                     (loop (cdr cur) (+ i 1) res)))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var)
           (set-nl-symbol-value! sym-idx saved-idx))))]
    [(_ (var list-expr break-cond) body ...)
     (let ([items list-expr]
           [saved-var (nl-symbol-value var)]
           [saved-idx (nl-symbol-value sym-idx)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([cur items] [i 0] [last-val nl-nil])
             (if (and (pair? cur) (not (null? cur)))
                 (begin
                   (set-nl-symbol-value! var (car cur))
                   (set-nl-symbol-value! sym-idx i)
                   (if (nl-truthy? break-cond)
                       last-val
                       (let ([res (begin body ...)])
                         (loop (cdr cur) (+ i 1) res))))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var)
           (set-nl-symbol-value! sym-idx saved-idx))))]))

(define-syntax nl-for
  (syntax-rules ()
    [(_ (var from-expr to-expr) body ...)
     (let ([from from-expr]
           [to to-expr]
           [saved-var (nl-symbol-value var)])
       (define step (if (> to from) 1 -1))
       (dynamic-wind
         void
         (lambda ()
           (let loop ([cur from] [last-val nl-nil])
             (if (if (> step 0) (<= cur to) (>= cur to))
                 (begin
                   (set-nl-symbol-value! var cur)
                   (let ([res (begin body ...)])
                     (loop (+ cur step) res)))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var))))]
    [(_ (var from-expr to-expr step-expr) body ...)
     (let ([from from-expr]
           [to to-expr]
           [step step-expr]
           [saved-var (nl-symbol-value var)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([cur from] [last-val nl-nil])
             (if (if (> step 0) (<= cur to) (>= cur to))
                 (begin
                   (set-nl-symbol-value! var cur)
                   (let ([res (begin body ...)])
                     (loop (+ cur step) res)))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var))))]
    [(_ (var from-expr to-expr step-expr break-cond) body ...)
     (let ([from from-expr]
           [to to-expr]
           [step step-expr]
           [saved-var (nl-symbol-value var)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([cur from] [last-val nl-nil])
             (if (if (> step 0) (<= cur to) (>= cur to))
                 (begin
                   (set-nl-symbol-value! var cur)
                   (if (nl-truthy? break-cond)
                       last-val
                       (let ([res (begin body ...)])
                         (loop (+ cur step) res))))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var))))]))

(define-syntax nl-dostring
  (syntax-rules ()
    [(_ (var str-expr) body ...)
     (let* ([s str-expr]
            [str (if (string? s) s (~a s))]
            [len (string-length str)]
            [saved-var (nl-symbol-value var)]
            [saved-idx (nl-symbol-value sym-idx)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([i 0] [last-val nl-nil])
             (if (< i len)
                 (begin
                   (set-nl-symbol-value! var (string (string-ref str i)))
                   (set-nl-symbol-value! sym-idx i)
                   (let ([res (begin body ...)])
                     (loop (+ i 1) res)))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var)
           (set-nl-symbol-value! sym-idx saved-idx))))]
    [(_ (var str-expr break-cond) body ...)
     (let* ([s str-expr]
            [str (if (string? s) s (~a s))]
            [len (string-length str)]
            [saved-var (nl-symbol-value var)]
            [saved-idx (nl-symbol-value sym-idx)])
       (dynamic-wind
         void
         (lambda ()
           (let loop ([i 0] [last-val nl-nil])
             (if (< i len)
                 (begin
                   (set-nl-symbol-value! var (string (string-ref str i)))
                   (set-nl-symbol-value! sym-idx i)
                   (if (nl-truthy? break-cond)
                       last-val
                       (let ([res (begin body ...)])
                         (loop (+ i 1) res))))
                 last-val)))
         (lambda ()
           (set-nl-symbol-value! var saved-var)
           (set-nl-symbol-value! sym-idx saved-idx))))]))

;; -------------------------------------------------------------------
;; Place Mutation Macros
;; -------------------------------------------------------------------

(define-syntax nl-++
  (syntax-rules ()
    [(_ sym)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0 (inexact->exact (truncate cur)))]
            [new-val (+ num 1)])
       (set-nl-symbol-value! sym new-val)
       new-val)]
    [(_ sym delta)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0 (inexact->exact (truncate cur)))]
            [new-val (+ num (inexact->exact (truncate delta)))])
       (set-nl-symbol-value! sym new-val)
       new-val)]))

(define-syntax nl---
  (syntax-rules ()
    [(_ sym)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0 (inexact->exact (truncate cur)))]
            [new-val (- num 1)])
       (set-nl-symbol-value! sym new-val)
       new-val)]
    [(_ sym delta)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0 (inexact->exact (truncate cur)))]
            [new-val (- num (inexact->exact (truncate delta)))])
       (set-nl-symbol-value! sym new-val)
       new-val)]))

(define-syntax nl-inc
  (syntax-rules ()
    [(_ sym)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0.0 (exact->inexact cur))]
            [new-val (+ num 1.0)])
       (set-nl-symbol-value! sym new-val)
       new-val)]
    [(_ sym delta)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0.0 (exact->inexact cur))]
            [new-val (+ num (exact->inexact delta))])
       (set-nl-symbol-value! sym new-val)
       new-val)]))

(define-syntax nl-dec
  (syntax-rules ()
    [(_ sym)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0.0 (exact->inexact cur))]
            [new-val (- num 1.0)])
       (set-nl-symbol-value! sym new-val)
       new-val)]
    [(_ sym delta)
     (let* ([cur (nl-symbol-value sym)]
            [num (if (nl-nil? cur) 0.0 (exact->inexact cur))]
            [new-val (- num (exact->inexact delta))])
       (set-nl-symbol-value! sym new-val)
       new-val)]))

(define-syntax nl-setq
  (syntax-rules ()
    [(_ var val)
     (let ([v val])
       (set-symbol-val! var v)
       v)]
    [(_ var val rest ...)
     (begin
       (set-symbol-val! var val)
       (nl-setq rest ...))]))

;; -------------------------------------------------------------------
;; Implicit Rest and Slicing on evaluated args
;; -------------------------------------------------------------------

(define (nl-fast-slice offset-int args)
  (cond
    [(= (length args) 1)
     (define target (car args))
     (cond
       [(list? target) (nl-slice-list target offset-int (- (length target) (max 0 offset-int)))]
       [(string? target) (nl-slice-string target offset-int (- (string-length target) (max 0 offset-int)))]
       [(nl-array? target)
        (make-nl-array (list (length (nl-array->list target)))
                       (nl-slice-list (nl-array->list target) offset-int (- (length (nl-array->list target)) (max 0 offset-int))))]
       [else (error 'eval "invalid target for implicit rest/slice: ~a" target)])]

    [(= (length args) 2)
     (define len-int (car args))
     (define target (cadr args))
     (cond
       [(list? target) (nl-slice-list target offset-int len-int)]
       [(string? target) (nl-slice-string target offset-int len-int)]
       [(nl-array? target)
        (define lst (nl-array->list target))
        (make-nl-array (list len-int) (nl-slice-list lst offset-int len-int))]
       [else (error 'eval "invalid target for implicit rest/slice: ~a" target)])]
    [else (error 'eval "invalid arguments for implicit rest/slice")]))

;; -------------------------------------------------------------------
;; Fast Function Application & Dispatch
;; -------------------------------------------------------------------

(define (nl-fast-call functor args)
  (cond
    ;; 1. Compiled Lambda
    [(and (nl-lambda? functor) (nl-lambda-compiled-proc functor))
     (apply (nl-lambda-compiled-proc functor) args)]

    ;; 2. Standard Lambda
    [(nl-lambda? functor)
     (apply-lambda functor args)]

    ;; 3. Builtin Primitive
    [(nl-primitive? functor)
     ((nl-primitive-proc functor) nl-eval args (current-context))]

    ;; 4. Context Functor (Default functor or Tree dictionary)
    [(nl-context? functor)
     (define def-functor (get-context-default-functor functor))
     (if (and def-functor (not (nl-nil? def-functor)))
         (parameterize ([current-context functor])
           (nl-fast-call def-functor args))
         ;; Tree dictionary with already evaluated args
         (case (length args)
           [(0)
            (for/list ([(k sym) (in-hash (nl-context-symbols functor))]
                       #:when (not (nl-nil? (nl-symbol-value sym))))
              (list (clean-tree-key k) (nl-symbol-value sym)))]
           [(1)
            (define key-str (make-tree-key-str (car args)))
            (define sym (hash-ref (nl-context-symbols functor) key-str #f))
            (if sym (nl-symbol-value sym) nl-nil)]
           [(2)
            (define key-str (make-tree-key-str (car args)))
            (define val (cadr args))
            (if (nl-nil? val)
                (begin (hash-remove! (nl-context-symbols functor) key-str) nl-nil)
                (let ([sym (hash-ref! (nl-context-symbols functor) key-str
                                      (lambda () (nl-symbol (nl-context-name functor) key-str nl-nil #f)))])
                  (set-nl-symbol-value! sym val)
                  val))]
           [else (error 'eval "invalid number of arguments for context tree functor: ~a" (length args))]))]

    ;; 5. List Implicit Indexing: (lst i ...)
    [(pair? functor)
     (nl-index-list functor args)]

    ;; 6. Array Implicit Indexing
    [(nl-array? functor)
     (if (null? args)
         functor
         (if (pair? (car args))
             (nl-array-ref functor (car args))
             (nl-array-ref functor args)))]

    ;; 7. String Implicit Indexing
    [(string? functor)
     (nl-index-string functor args)]

    ;; 8. Integer Implicit Rest / Slice: (1 lst) or (2 3 lst)
    [(exact-integer? functor)
     (nl-fast-slice functor args)]

    [else
     (error 'eval "cannot apply value as function: ~a" functor)]))

;; FOOP dispatch
(define (nl-foop-dispatch method-sym target-sym-or-expr target-obj args)
  (unless (and (pair? target-obj) (or (nl-symbol? (car target-obj)) (nl-context? (car target-obj))))
    (error 'eval "FOOP target must be an object list (Class ...): ~a" target-obj))
  (define class-head (car target-obj))
  (define class-ctx
    (if (nl-context? class-head)
        class-head
        (get-or-create-context (nl-symbol-name class-head))))
  (define clean-name (if (string? method-sym) method-sym (let ([s (~a method-sym)]) (if (string-prefix? s ":") (substring s 1) s))))
  (define method-func (hash-ref (nl-context-symbols class-ctx) clean-name #f))
  (unless method-func
    (error 'eval "method ~a not found in class ~a" clean-name (nl-context-name class-ctx)))
  (define func-val (nl-symbol-value method-func))
  (define mut-obj target-obj)
  (define is-sym? (nl-symbol? target-sym-or-expr))
  (define (update-target-place! new-val)
    (set! mut-obj new-val)
    (when is-sym?
      (set-nl-symbol-value! target-sym-or-expr new-val)))

  (define prev-target (current-self-target))
  (define prev-updater (current-self-updater))
  (define prev-ctx (current-context))

  (current-self-target mut-obj)
  (current-self-updater update-target-place!)
  (current-context class-ctx)

  (define res
    (if (and (nl-lambda? func-val) (nl-lambda-compiled-proc func-val))
        (apply (nl-lambda-compiled-proc func-val) args)
        (nl-fast-call func-val args)))

  (update-target-place! (current-self-target))

  (current-self-target prev-target)
  (current-self-updater prev-updater)
  (current-context prev-ctx)
  res)

;; Dynamic scoping binder for lambdas
(define (bind-and-run params-syms args-list target-ctx thunk)
  (define saved-ctx (current-context))
  (define saved-bindings
    (for/list ([s params-syms])
      (cons s (nl-symbol-value s))))
  (define saved-args (current-call-args))
  (dynamic-wind
    (lambda ()
      (current-context target-ctx)
      (current-call-args (if (> (length args-list) (length params-syms))
                             (drop args-list (length params-syms))
                             '()))
      (let loop ([syms params-syms] [args args-list])
        (when (pair? syms)
          (define val (if (pair? args) (car args) nl-nil))
          (set-nl-symbol-value! (car syms) val)
          (loop (cdr syms) (if (pair? args) (cdr args) '())))))
    thunk
    (lambda ()
      (for ([b saved-bindings])
        (set-nl-symbol-value! (car b) (cdr b)))
      (current-call-args saved-args)
      (current-context saved-ctx))))

;; -------------------------------------------------------------------
;; FOOP Self Place Helpers & Fast Primitives
;; -------------------------------------------------------------------

(define (nl-self-ref . idxs)
  (define cur (current-self-target))
  (if (null? idxs)
      cur
      (nl-index-list cur idxs)))

(define (nl-self-inc-dec! op idx [delta #f])
  (define cur-target (current-self-target))
  (unless (list? cur-target)
    (error 'self "current FOOP target is not a list: ~a" cur-target))
  (define norm-idx (if (< idx 0) (+ (length cur-target) idx) idx))
  (define old-val (list-ref cur-target norm-idx))
  (define new-val
    (case op
      [(++)
       (define d (if delta (inexact->exact (truncate delta)) 1))
       (define base (if (number? old-val) (inexact->exact (truncate old-val)) 0))
       (+ base d)]
      [(--)
       (define d (if delta (inexact->exact (truncate delta)) 1))
       (define base (if (number? old-val) (inexact->exact (truncate old-val)) 0))
       (- base d)]
      [(inc)
       (define d (if delta (exact->inexact delta) 1.0))
       (define base (if (number? old-val) (exact->inexact old-val) 0.0))
       (+ base d)]
      [(dec)
       (define d (if delta (exact->inexact delta) 1.0))
       (define base (if (number? old-val) (exact->inexact old-val) 0.0))
       (- base d)]))
  (define new-target (list-set-path cur-target (list norm-idx) new-val))
  (current-self-target new-target)
  ((current-self-updater) new-target)
  new-val)

(define (nl-self-setf! idx val)
  (define cur-target (current-self-target))
  (define new-target (list-set-path cur-target (if (list? idx) idx (list idx)) val))
  (current-self-target new-target)
  ((current-self-updater) new-target)
  val)

(define (nl-fast-map fn . lsts)
  (cond
    [(and (null? (cdr lsts)) (nl-lambda? fn) (nl-lambda-compiled-proc fn))
     (define proc (nl-lambda-compiled-proc fn))
     (map proc (car lsts))]
    [(and (null? (cdr lsts)) (procedure? fn))
     (map fn (car lsts))]
    [else
     (apply map (lambda xs (nl-fast-call fn xs)) lsts)]))

(define (nl-fast-filter fn lst)
  (cond
    [(and (nl-lambda? fn) (nl-lambda-compiled-proc fn))
     (define proc (nl-lambda-compiled-proc fn))
     (filter (lambda (x) (nl-truthy? (proc x))) lst)]
    [(procedure? fn)
     (filter (lambda (x) (nl-truthy? (fn x))) lst)]
    [else
     (filter (lambda (x) (nl-truthy? (nl-fast-call fn (list x)))) lst)]))

(define (nl-fast-clean fn lst)
  (cond
    [(and (nl-lambda? fn) (nl-lambda-compiled-proc fn))
     (define proc (nl-lambda-compiled-proc fn))
     (filter (lambda (x) (not (nl-truthy? (proc x)))) lst)]
    [(procedure? fn)
     (filter (lambda (x) (not (nl-truthy? (fn x)))) lst)]
    [else
     (filter (lambda (x) (not (nl-truthy? (nl-fast-call fn (list x))))) lst)]))

(define (nl-fast-length x)
  (cond
    [(list? x) (length x)]
    [(string? x) (string-length x)]
    [(nl-array? x) (apply * (nl-array-dims x))]
    [else 0]))

(define (nl-fast-sequence from to [step #f])
  (define s (or step (if (> to from) 1 -1)))
  (if (> s 0)
      (for/list ([i (in-range from (+ to 1) s)]) i)
      (for/list ([i (in-range from (- to 1) s)]) i)))
