#lang racket/base

(require racket/string
         racket/list
         racket/hash
         racket/format
         "nl-types.rkt")

(provide (all-defined-out))

;; -------------------------------------------------------------------
;; Global Context Registry & Symbol Table
;; -------------------------------------------------------------------

(define global-contexts (make-hash))
(define main-context (nl-context "MAIN" (make-hash) nl-nil #f))
(hash-set! global-contexts "MAIN" main-context)
(define current-context (make-parameter main-context))

;; Find or create symbol
(define (find-or-create-symbol sym-name [ctx #f])
  (cond
    [(nl-symbol? sym-name) sym-name]
    [(string? sym-name)
     (if (and (string-contains? sym-name ":") (not (string=? sym-name ":")))
         (let* ([parts (string-split sym-name ":" #:trim? #f)]
                [ctx-name (if (string=? (car parts) "") "MAIN" (car parts))]
                [name (string-join (cdr parts) ":")]
                [target-ctx (get-or-create-context ctx-name)])
           (hash-ref! (nl-context-symbols target-ctx) name
                      (lambda ()
                        (nl-symbol ctx-name name nl-nil #f))))
         (if ctx
             ;; Explicit context given: find or create directly in that context
             (hash-ref! (nl-context-symbols ctx) sym-name
                        (lambda ()
                          (nl-symbol (nl-context-name ctx) sym-name nl-nil #f)))
             ;; No explicit context: check current-context, then fallback to MAIN
             (let* ([active-ctx (current-context)]
                    [symbols (nl-context-symbols active-ctx)])
               (cond
                 [(hash-has-key? symbols sym-name)
                  (hash-ref symbols sym-name)]
                 [(and (not (string=? (nl-context-name active-ctx) "MAIN"))
                       (hash-has-key? (nl-context-symbols main-context) sym-name))
                  (hash-ref (nl-context-symbols main-context) sym-name)]
                 [else
                  (define new-sym (nl-symbol (nl-context-name active-ctx) sym-name nl-nil #f))
                  (hash-set! symbols sym-name new-sym)
                  new-sym]))))]
    [else sym-name]))

(define (get-or-create-context name [default-functor nl-nil])
  (hash-ref! global-contexts name
             (lambda ()
               (define ctx (nl-context name (make-hash) default-functor #f))
               (unless (string=? name "MAIN")
                 (define sym (find-or-create-symbol name main-context))
                 (set-nl-symbol-value! sym ctx))
               ctx)))

;; Pre-populate Tree and Class contexts
(define tree-context (get-or-create-context "Tree" nl-nil))
(define class-context (get-or-create-context "Class"))

;; System symbols
(define sym-it (find-or-create-symbol "$it" main-context))
(define sym-idx (find-or-create-symbol "$idx" main-context))
(define sym-args (find-or-create-symbol "$args" main-context))
(define sym-main-args (find-or-create-symbol "main-args" main-context))
(define sym-dollar-main-args (find-or-create-symbol "$main-args" main-context))

(define (set-symbol-val! sym val)
  (if (nl-symbol-protected? sym)
      (error 'eval "symbol is protected: ~a" (nl-symbol-name sym))
      (begin
        (set-nl-symbol-value! sym val)
        (when (string=? (nl-symbol-name sym) (nl-symbol-context-name sym))
          (define ctx (hash-ref global-contexts (nl-symbol-context-name sym) #f))
          (when ctx
            (set-nl-context-default-functor! ctx val))))))

;; -------------------------------------------------------------------
;; Dynamic Scoping Mechanism
;; -------------------------------------------------------------------

(define (with-dynamic-bindings bindings thunk)
  ;; bindings: list of (cons nl-symbol new-val)
  (define saved
    (for/list ([b bindings])
      (cons (car b) (nl-symbol-value (car b)))))
  (dynamic-wind
   (lambda ()
     (for ([b bindings])
       (set-nl-symbol-value! (car b) (cdr b))))
   thunk
   (lambda ()
     (for ([s saved])
       (set-nl-symbol-value! (car s) (cdr s))))))

;; Current call unbound args parameter
(define current-call-args (make-parameter '()))

;; Current FOOP target and place
(define current-self-target (make-parameter nl-nil))
(define current-self-updater (make-parameter void))

;; Prompt tag for throw/catch
(define nl-catch-prompt-tag (make-continuation-prompt-tag 'nl-catch))

(struct nl-throw-exn (val) #:transparent)

;; -------------------------------------------------------------------
;; AST Symbol Resolution (Converts strings from reader to nl-symbols)
;; -------------------------------------------------------------------

(define (resolve-ast-symbols ast [ctx #f])
  (cond
    [(string? ast) ast]
    [(nl-symbol? ast) ast]
    [(null? ast) '()]
    [(pair? ast)
     (cons (resolve-ast-symbols (car ast) ctx)
           (resolve-ast-symbols (cdr ast) ctx))]
    [else ast]))

;; -------------------------------------------------------------------
;; Evaluator: nl-eval
;; -------------------------------------------------------------------

(define (nl-eval expr [ctx #f])
  (when ctx (current-context ctx))
  (cond
    ;; Self-evaluating types
    [(nl-nil? expr) nl-nil]
    [(nl-true? expr) nl-true]
    [(number? expr) expr]
    [(string? expr) expr]
    [(nl-array? expr) expr]
    [(nl-context? expr) expr]
    [(nl-primitive? expr) expr]
    [(nl-lambda? expr) expr]
    [(null? expr) '()]

    ;; Symbol lookup: returns current dynamic value
    [(nl-symbol? expr)
     (nl-symbol-value expr)]

    ;; List expression: (functor . args)
    [(pair? expr)
     (define raw-functor (car expr))
     (define raw-args (cdr expr))

     ;; Check for special forms dispatch by name
     (define sf-name (and (nl-symbol? raw-functor) (nl-symbol-name raw-functor)))
     (cond
       ;; Special Form: quote
       [(and sf-name (string=? sf-name "quote"))
        (if (pair? raw-args) (car raw-args) '())]

       ;; Special Form: if
       [(and sf-name (string=? sf-name "if"))
        (eval-if raw-args)]

       ;; Special Form: when
       [(and sf-name (string=? sf-name "when"))
        (if (null? raw-args)
            nl-nil
            (let ([cond-val (nl-eval (car raw-args))])
              (set-symbol-val! sym-it cond-val)
              (if (nl-truthy? cond-val)
                  (eval-body (cdr raw-args))
                  nl-nil)))]

       ;; Special Form: unless
       [(and sf-name (string=? sf-name "unless"))
        (if (null? raw-args)
            nl-nil
            (let ([cond-val (nl-eval (car raw-args))])
              (set-symbol-val! sym-it cond-val)
              (if (not (nl-truthy? cond-val))
                  (eval-body (cdr raw-args))
                  nl-nil)))]

       ;; Special Form: cond
       [(and sf-name (string=? sf-name "cond"))
        (eval-cond raw-args)]

       ;; Special Form: case
       [(and sf-name (string=? sf-name "case"))
        (eval-case raw-args)]

       ;; Special Form: set / setq / setf
       [(and sf-name (or (string=? sf-name "set")
                         (string=? sf-name "setq")
                         (string=? sf-name "setf")))
        (eval-set-forms sf-name raw-args)]

       ;; Special Form: define
       [(and sf-name (string=? sf-name "define"))
        (eval-define raw-args #f)]

       ;; Special Form: define-macro
       [(and sf-name (string=? sf-name "define-macro"))
        (eval-define raw-args #t)]

       ;; Special Form: macro (expansion macro)
       [(and sf-name (string=? sf-name "macro"))
        (eval-macro-def raw-args)]

       ;; Special Form: lambda / fn
       [(and sf-name (or (string=? sf-name "lambda")
                         (string=? sf-name "fn")))
        (eval-lambda-constructor raw-args #f)]

       ;; Special Form: lambda-macro
       [(and sf-name (string=? sf-name "lambda-macro"))
        (eval-lambda-constructor raw-args #t)]

       ;; Special Form: let
       [(and sf-name (string=? sf-name "let"))
        (eval-let raw-args #f)]

       ;; Special Form: letn
       [(and sf-name (string=? sf-name "letn"))
        (eval-let raw-args #t)]

       ;; Special Form: letex
       [(and sf-name (string=? sf-name "letex"))
        (eval-letex raw-args)]

       ;; Special Form: expand
       [(and sf-name (string=? sf-name "expand"))
        (eval-expand raw-args)]

       ;; Special Form: while
       [(and sf-name (string=? sf-name "while"))
        (eval-while raw-args)]

       ;; Special Form: until
       [(and sf-name (string=? sf-name "until"))
        (eval-until raw-args)]

       ;; Special Form: do-while
       [(and sf-name (string=? sf-name "do-while"))
        (eval-do-while raw-args)]

       ;; Special Form: do-until
       [(and sf-name (string=? sf-name "do-until"))
        (eval-do-until raw-args)]

       ;; Special Form: dotimes
       [(and sf-name (string=? sf-name "dotimes"))
        (eval-dotimes raw-args)]

       ;; Special Form: dolist
       [(and sf-name (string=? sf-name "dolist"))
        (eval-dolist raw-args)]

       ;; Special Form: dostring
       [(and sf-name (string=? sf-name "dostring"))
        (eval-dostring raw-args)]

       ;; Special Form: dotree
       [(and sf-name (string=? sf-name "dotree"))
        (eval-dotree raw-args)]

       ;; Special Form: for
       [(and sf-name (string=? sf-name "for"))
        (eval-for raw-args)]

       ;; Special Form: catch
       [(and sf-name (string=? sf-name "catch"))
        (eval-catch raw-args)]

       ;; Special Form: throw
       [(and sf-name (string=? sf-name "throw"))
        (define val (if (pair? raw-args) (nl-eval (car raw-args)) nl-nil))
        (abort-current-continuation nl-catch-prompt-tag (nl-throw-exn val))]

       ;; Special Form: begin
       [(and sf-name (string=? sf-name "begin"))
        (eval-body raw-args)]

       ;; Special Form: silent
       [(and sf-name (string=? sf-name "silent"))
        (eval-body raw-args)
        nl-nil]

       ;; Special Form: and
       [(and sf-name (string=? sf-name "and"))
        (eval-and raw-args)]

       ;; Special Form: or
       [(and sf-name (string=? sf-name "or"))
        (eval-or raw-args)]

       ;; Special Form: curry
       [(and sf-name (string=? sf-name "curry"))
        (eval-curry raw-args)]

       ;; Special Form: : (FOOP Colon Operator)
       [(and sf-name (string=? sf-name ":"))
        (eval-foop-colon raw-args)]

       ;; Special Form: bind
       [(and sf-name (string=? sf-name "bind"))
        (eval-bind raw-args)]

       ;; Special Form: constant
       [(and sf-name (string=? sf-name "constant"))
        (eval-constant raw-args)]

       ;; Special Form: def-new
       [(and sf-name (string=? sf-name "def-new"))
        (eval-def-new raw-args)]

       ;; Special Form: default
       [(and sf-name (string=? sf-name "default"))
        (eval-default raw-args)]

       ;; Special Form: doargs
       [(and sf-name (string=? sf-name "doargs"))
        (eval-doargs raw-args)]

       ;; Special Form: collect
       [(and sf-name (string=? sf-name "collect"))
        (eval-collect raw-args)]

       ;; Special Form: local
       [(and sf-name (string=? sf-name "local"))
        (eval-local raw-args)]

       ;; Special Form: global
       [(and sf-name (string=? sf-name "global"))
        (eval-global raw-args)]

       ;; Otherwise, evaluate the functor!
       [else
        (define functor (nl-eval raw-functor))
        (apply-functor functor raw-args)])]

    [else expr]))

;; -------------------------------------------------------------------
;; Functor Application Engine
;; -------------------------------------------------------------------

(define (apply-functor functor raw-args)
  (cond
    ;; 1. Primitive procedure
    [(nl-primitive? functor)
     (if (nl-primitive-is-special? functor)
         ;; Special primitive: receives unevaluated args
         ((nl-primitive-proc functor) nl-eval raw-args (current-context))
         ;; Standard primitive: evaluates args first
         (let ([evaluated-args (map nl-eval raw-args)])
           ((nl-primitive-proc functor) nl-eval evaluated-args (current-context))))]

    ;; 2. Lambda function (evaluates arguments, dynamically binds parameters)
    [(and (nl-lambda? functor) (not (nl-lambda-is-macro? functor)))
     (define evaluated-args (map nl-eval raw-args))
     (apply-lambda functor evaluated-args)]

    ;; 3. Lambda-macro / Fexpr (unevaluated arguments passed directly)
    [(and (nl-lambda? functor) (nl-lambda-is-macro? functor))
     (apply-lambda functor raw-args)]

    ;; 4. Context Functor (Default functor or Tree dictionary)
    [(nl-context? functor)
     (apply-context-functor functor raw-args)]

    ;; 5. List (Implicit Indexing: (lst i ...))
    [(pair? functor)
     (define evaluated-indices (map nl-eval raw-args))
     (nl-index-list functor evaluated-indices)]

    ;; 6. Array (Implicit Indexing: (arr i ...))
    [(nl-array? functor)
     (define evaluated-indices (map nl-eval raw-args))
     (if (null? evaluated-indices)
         functor
         (if (pair? (car evaluated-indices))
             ;; Index vector passed as a single list: (arr '(i j))
             (nl-array-ref functor (car evaluated-indices))
             (nl-array-ref functor evaluated-indices)))]

    ;; 7. String (Implicit Indexing: ("str" i))
    [(string? functor)
     (define evaluated-args (map nl-eval raw-args))
     (nl-index-string functor evaluated-args)]

    ;; 8. Integer (Implicit Rest / Slice: (1 lst) or (2 3 lst))
    [(exact-integer? functor)
     (apply-implicit-slice functor raw-args)]

    [else
     (error 'eval "invalid function or functor: ~a" (nl->string functor))]))

(define (apply-evaluated functor actual-args)
  (cond
    [(nl-primitive? functor)
     ((nl-primitive-proc functor) nl-eval actual-args (current-context))]
    [(nl-lambda? functor)
     (apply-lambda functor actual-args)]
    [(nl-context? functor)
     (apply-context-functor functor (map (lambda (a) (list 'quote a)) actual-args))]
    [(pair? functor)
     (nl-index-list functor actual-args)]
    [(nl-array? functor)
     (if (null? actual-args)
         functor
         (if (pair? (car actual-args))
             (nl-array-ref functor (car actual-args))
             (nl-array-ref functor actual-args)))]
    [(string? functor)
     (nl-index-string functor actual-args)]
    [(exact-integer? functor)
     (apply-implicit-slice functor (map (lambda (a) (list 'quote a)) actual-args))]
    [else
     (error 'eval "invalid function: ~a" (nl->string functor))]))

;; -------------------------------------------------------------------
;; Lambda Application
;; -------------------------------------------------------------------

(define (apply-lambda lam actual-args)
  (define params (nl-lambda-params lam))
  (define body (nl-lambda-body lam))
  ;; Match parameters with actual arguments
  (define-values (bindings unbound)
    (bind-parameters params actual-args))
  (parameterize ([current-call-args unbound]
                 [current-context (get-or-create-context (nl-lambda-ctx-name lam))])
    (with-dynamic-bindings bindings
      (lambda ()
        (eval-body body)))))

(define (bind-parameters params actual-args)
  (let loop ([ps params] [as actual-args] [bindings '()])
    (cond
      [(null? ps)
       (values (reverse bindings) as)]
      [(nl-symbol? (car ps))
       (define p-sym (car ps))
       (if (pair? as)
           (loop (cdr ps) (cdr as) (cons (cons p-sym (car as)) bindings))
           ;; Argument missing: default to nil
           (loop (cdr ps) '() (cons (cons p-sym nl-nil) bindings)))]
      [(pair? (car ps))
       ;; Parameter with default expression: (param default-exp)
       (define p-pair (car ps))
       (define p-sym (car p-pair))
       (define p-default (cadr p-pair))
       (if (pair? as)
           (loop (cdr ps) (cdr as) (cons (cons p-sym (car as)) bindings))
           (let ([def-val (nl-eval p-default)])
             (loop (cdr ps) '() (cons (cons p-sym def-val) bindings))))]
      [else
       (error 'eval "invalid parameter in lambda: ~a" (car ps))])))

(define (eval-body body)
  (if (null? body)
      nl-nil
      (let loop ([exprs body])
        (if (null? (cdr exprs))
            (nl-eval (car exprs))
            (begin
              (nl-eval (car exprs))
              (loop (cdr exprs)))))))

;; -------------------------------------------------------------------
;; Context Functor Application
;; -------------------------------------------------------------------

(define (get-context-default-functor ctx)
  (define def-functor (nl-context-default-functor ctx))
  (if (and def-functor (not (nl-nil? def-functor)))
      def-functor
      (let ([sym (hash-ref (nl-context-symbols ctx) (nl-context-name ctx) #f)])
        (if (and sym (not (nl-nil? (nl-symbol-value sym))))
            (nl-symbol-value sym)
            nl-nil))))

(define (apply-context-functor ctx raw-args)
  (define def-functor (get-context-default-functor ctx))
  (cond
    ;; If default functor is defined and not nil, call it!
    [(and def-functor (not (nl-nil? def-functor)))
     (parameterize ([current-context ctx])
       (apply-functor def-functor raw-args))]

    ;; If default functor is nil or undefined: acts as Hash / Tree Dictionary
    [else
     (define evaluated-args (map nl-eval raw-args))
     (case (length evaluated-args)
       ;; 0 args: returns alist of all key-value pairs
       [(0)
        (for/list ([(k sym) (in-hash (nl-context-symbols ctx))]
                   #:when (not (nl-nil? (nl-symbol-value sym))))
          (list (clean-tree-key k) (nl-symbol-value sym)))]

       ;; 1 arg: retrieve value for key
       [(1)
        (define key-str (make-tree-key-str (car evaluated-args)))
        (define sym (hash-ref (nl-context-symbols ctx) key-str #f))
        (if sym (nl-symbol-value sym) nl-nil)]

       ;; 2 args: set value for key (if nil, delete key)
       [(2)
        (define key-str (make-tree-key-str (car evaluated-args)))
        (define val (cadr evaluated-args))
        (if (nl-nil? val)
            (begin
              (hash-remove! (nl-context-symbols ctx) key-str)
              nl-nil)
            (let ([sym (hash-ref! (nl-context-symbols ctx) key-str
                                  (lambda ()
                                    (nl-symbol (nl-context-name ctx) key-str nl-nil #f)))])
              (set-nl-symbol-value! sym val)
              val))]
       [else
        (error 'eval "too many arguments for dictionary context ~a" (nl-context-name ctx))])]))

(define (make-tree-key-str k)
  (string-append "_" (if (string? k) k (~a k))))

(define (clean-tree-key k)
  (if (string-prefix? k "_")
      (substring k 1)
      k))

;; -------------------------------------------------------------------
;; Implicit Indexing & Slicing
;; -------------------------------------------------------------------

(define (nl-index-list lst indices)
  (if (null? indices)
      lst
      (if (and (= (length indices) 1) (pair? (car indices)))
          ;; Index vector: (lst '(3 1))
          (nl-index-list-vector lst (car indices))
          (nl-index-list-vector lst indices))))

(define (nl-index-list-vector lst vec)
  (if (null? vec)
      lst
      (let loop ([cur lst] [idxs vec])
        (if (null? idxs)
            cur
            (let* ([idx (car idxs)]
                   [len (if (list? cur) (length cur) 0)]
                   [norm-idx (if (< idx 0) (+ len idx) idx)])
              (when (or (< norm-idx 0) (>= norm-idx len))
                (error 'eval "index out of bounds in list: ~a" idx))
              (loop (list-ref cur norm-idx) (cdr idxs)))))))

(define (nl-index-string str args)
  (cond
    [(null? args) str]
    [(= (length args) 1)
     (define idx (car args))
     (define len (string-length str))
     (define norm-idx (if (< idx 0) (+ len idx) idx))
     (if (or (< norm-idx 0) (>= norm-idx len))
         nl-nil
         (string (string-ref str norm-idx)))]
    [(= (length args) 2)
     ;; (slice str offset length)
     (nl-slice-string str (car args) (cadr args))]
    [else
     (error 'eval "invalid string indexing: ~a" args)]))

(define (apply-implicit-slice offset-int raw-args)
  (define evaluated-args (map nl-eval raw-args))
  (cond
    [(= (length evaluated-args) 1)
     ;; Offset only: (1 lst) -> rest of lst
     (define target (car evaluated-args))
     (cond
       [(list? target) (nl-slice-list target offset-int (- (length target) (max 0 offset-int)))]
       [(string? target) (nl-slice-string target offset-int (- (string-length target) (max 0 offset-int)))]
       [(nl-array? target)
        (make-nl-array (list (length (nl-array->list target)))
                       (nl-slice-list (nl-array->list target) offset-int (- (length (nl-array->list target)) (max 0 offset-int))))]
       [else (error 'eval "invalid target for implicit rest/slice: ~a" target)])]

    [(= (length evaluated-args) 2)
     ;; Offset and target, where second arg is length: (offset len target)
     (define len-int (car evaluated-args))
     (define target (cadr evaluated-args))
     (cond
       [(list? target) (nl-slice-list target offset-int len-int)]
       [(string? target) (nl-slice-string target offset-int len-int)]
       [(nl-array? target)
        (define lst (nl-array->list target))
        (make-nl-array (list (length lst)) (nl-slice-list lst offset-int len-int))]
       [else (error 'eval "invalid target for implicit slice: ~a" target)])]

    [else
     (error 'eval "invalid arguments for implicit slice: ~a" raw-args)]))

(define (nl-slice-list lst offset [len #f])
  (define n (length lst))
  (define start (if (< offset 0) (max 0 (+ n offset)) (min n offset)))
  (define remaining (- n start))
  (define count
    (if len
        (if (< len 0)
            (max 0 (+ remaining len))
            (min remaining len))
        remaining))
  (take (drop lst start) count))

(define (nl-slice-string str offset [len #f])
  (define n (string-length str))
  (define start (if (< offset 0) (max 0 (+ n offset)) (min n offset)))
  (define remaining (- n start))
  (define count
    (if len
        (if (< len 0)
            (max 0 (+ remaining len))
            (min remaining len))
        remaining))
  (substring str start (+ start count)))

;; -------------------------------------------------------------------
;; FOOP Colon Operator: (: method-name target-obj args...)
;; -------------------------------------------------------------------

(define (eval-foop-colon args)
  (when (< (length args) 2)
    (error 'eval "syntax error in colon operator: (: method obj ...); got ~a" args))
  (define method-expr (car args))
  (define target-expr (cadr args))
  (define method-args (cddr args))

  (define method-name
    (cond
      [(nl-symbol? method-expr) (nl-symbol-name method-expr)]
      [(string? method-expr) method-expr]
      [else (error 'eval "invalid method name in colon operator: ~a" method-expr)]))

  ;; Evaluate target object
  (define target-obj (nl-eval target-expr))
  (unless (and (pair? target-obj) (or (nl-symbol? (car target-obj)) (nl-context? (car target-obj))))
    (error 'eval "invalid FOOP object: ~a" target-obj))

  (define class-name
    (if (nl-symbol? (car target-obj))
        (nl-symbol-name (car target-obj))
        (nl-context-name (car target-obj))))

  (define class-ctx (get-or-create-context class-name))
  (define method-sym
    (hash-ref (nl-context-symbols class-ctx) method-name
              (lambda ()
                ;; Check MAIN context fallback
                (hash-ref (nl-context-symbols main-context)
                          (string-append class-name ":" method-name)
                          (lambda ()
                            (error 'eval "method ~a not found in class ~a" method-name class-name))))))

  (define method-func (nl-symbol-value method-sym))
  (unless (or (nl-lambda? method-func) (nl-primitive? method-func))
    (error 'eval "method ~a in ~a is not callable: ~a" method-name class-name method-func))

  ;; Place update support for mutable FOOP objects:
  ;; If target was a symbol (e.g. aCircle), we can write back changes to it!
  (define mut-obj target-obj)
  (define (update-target-place! new-val)
    (set! mut-obj new-val)
    (when (nl-symbol? target-expr)
      (set-symbol-val! target-expr new-val)))

  (parameterize ([current-self-target mut-obj]
                 [current-self-updater update-target-place!])
    (define evaluated-method-args (map nl-eval method-args))
    (define res
      (if (nl-lambda? method-func)
          (apply-lambda method-func evaluated-method-args)
          ((nl-primitive-proc method-func) nl-eval evaluated-method-args (current-context))))
    (update-target-place! (current-self-target))
    res))

;; -------------------------------------------------------------------
;; Place Mutation Engine: set, setq, setf
;; -------------------------------------------------------------------

(define (eval-set-forms sf-name args)
  (cond
    [(string=? sf-name "set")
     ;; (set 'sym val ['sym2 val2 ...])
     ;; In `set`, the place expression is evaluated to a symbol!
     (let loop ([pairs args] [last-val nl-nil])
       (if (null? pairs)
           last-val
           (let* ([sym-expr (car pairs)]
                  [val-expr (if (pair? (cdr pairs)) (cadr pairs) nl-nil)]
                  [sym (nl-eval sym-expr)]
                  [val (nl-eval val-expr)])
             (unless (nl-symbol? sym)
               (error 'set "expected a symbol, got ~a" sym))
             (set-symbol-val! sym val)
             (loop (cddr pairs) val))))]

    [else
     ;; `setq` / `setf`: place is NOT evaluated if a symbol!
     (let loop ([pairs args] [last-val nl-nil])
       (if (null? pairs)
           last-val
           (let* ([place-expr (car pairs)]
                  [val-expr (if (pair? (cdr pairs)) (cadr pairs) nl-nil)]
                  [val (nl-eval val-expr)])
             (mutate-place! place-expr val)
             (loop (cddr pairs) val))))]))

(define (mutate-place! place-expr val)
  (cond
    ;; 1. Simple Symbol place: (setq x 10)
    [(nl-symbol? place-expr)
     (set-symbol-val! place-expr val)]

    ;; 2. Place expression: (nth idx target) or (target idx ...) or (first target) or (assoc key target)
    [(pair? place-expr)
     (define op (car place-expr))
     (define op-name (and (nl-symbol? op) (nl-symbol-name op)))
     (cond
       ;; (nth idx target)
       [(and op-name (string=? op-name "nth"))
        (define idx (nl-eval (cadr place-expr)))
        (define target-sym (caddr place-expr))
        (define target (nl-eval target-sym))
        (cond
          [(nl-array? target)
           (nl-array-set! target (if (pair? idx) idx (list idx)) val)]
          [(list? target)
           (define new-list (list-set-path target (if (pair? idx) idx (list idx)) val))
           (update-container! target-sym new-list)]
          [(string? target)
           (define new-str (string-set-index target idx val))
           (update-container! target-sym new-str)]
          [else (error 'setf "cannot set nth of ~a" target)])]

       ;; (first target)
       [(and op-name (string=? op-name "first"))
        (define target-sym (cadr place-expr))
        (define target (nl-eval target-sym))
        (if (pair? target)
            (update-container! target-sym (cons val (cdr target)))
            (error 'setf "cannot set first of ~a" target))]

       ;; (last target)
       [(and op-name (string=? op-name "last"))
        (define target-sym (cadr place-expr))
        (define target (nl-eval target-sym))
        (if (pair? target)
            (update-container! target-sym (append (drop-right target 1) (list val)))
            (error 'setf "cannot set last of ~a" target))]

       ;; (assoc key-expr target)
       [(and op-name (string=? op-name "assoc"))
        (define key-val (nl-eval (cadr place-expr)))
        (define target-sym (caddr place-expr))
        (define target (nl-eval target-sym))
        (define new-target
          (map (lambda (pair)
                 (if (and (pair? pair) (equal? (car pair) key-val))
                     val
                     pair))
               target))
        (update-container! target-sym new-target)]

       ;; (lookup key-expr target)
       [(and op-name (string=? op-name "lookup"))
        (define key-val (nl-eval (cadr place-expr)))
        (define target-sym (caddr place-expr))
        (define target (nl-eval target-sym))
        (define new-target
          (map (lambda (pair)
                 (if (and (pair? pair) (equal? (car pair) key-val))
                     (list (car pair) val)
                     pair))
               target))
        (update-container! target-sym new-target)]

       ;; (self idx ...) inside FOOP
       [(and op-name (string=? op-name "self"))
        (define indices (map nl-eval (cdr place-expr)))
        (define target (current-self-target))
        (define new-target (list-set-path target indices val))
        (current-self-target new-target)
        ((current-self-updater) new-target)]

       ;; Context indexing: (MyList idx)
       [(and (nl-symbol? op) (nl-context? (nl-symbol-value op)))
        (define ctx (nl-symbol-value op))
        (define idx (nl-eval (cadr place-expr)))
        (define def-sym (hash-ref (nl-context-symbols ctx) (nl-context-name ctx) #f))
        (when def-sym
          (define cur (nl-symbol-value def-sym))
          (when (list? cur)
            (define new-list (list-set-path cur (list idx) val))
            (set-nl-symbol-value! def-sym new-list)))]

       ;; Implicit indexing: (target-sym idx ...)
       [(nl-symbol? op)
        (define target (nl-eval op))
        (define indices (map nl-eval (cdr place-expr)))
        (cond
          [(nl-array? target)
           (nl-array-set! target indices val)]
          [(list? target)
           (define new-list (list-set-path target indices val))
           (update-container! op new-list)]
          [(string? target)
           (define new-str (string-set-index target (car indices) val))
           (update-container! op new-str)]
          [else
           (error 'setf "invalid target for indexing: ~a" target)])]

       [else
        (error 'setf "invalid place expression: ~a" place-expr)])]

    [else
     (error 'setf "invalid place for mutation: ~a" place-expr)]))

(define (update-container! target-sym new-val)
  (cond
    [(nl-symbol? target-sym)
     (set-symbol-val! target-sym new-val)]
    [(pair? target-sym)
     (mutate-place! target-sym new-val)]
    [else
     (void)]))

(define (list-set-path lst indices val)
  (if (null? indices)
      val
      (let* ([idx (car indices)]
             [len (length lst)]
             [norm-idx (if (< idx 0) (+ len idx) idx)])
        (when (or (< norm-idx 0) (>= norm-idx len))
          (error 'setf "index out of bounds: ~a" idx))
        (for/list ([elem lst] [i (in-naturals)])
          (if (= i norm-idx)
              (if (null? (cdr indices))
                  val
                  (list-set-path elem (cdr indices) val))
              elem)))))

(define (string-set-index str idx val-str)
  (define len (string-length str))
  (define norm-idx (if (< idx 0) (+ len idx) idx))
  (define rep (if (string? val-str) val-str (~a val-str)))
  (string-append (substring str 0 norm-idx)
                 rep
                 (substring str (+ norm-idx 1))))

;; -------------------------------------------------------------------
;; Special Forms Implementation
;; -------------------------------------------------------------------

(define (eval-if args)
  (cond
    [(null? args) nl-nil]
    ;; Standard 2 or 3 argument if: (if cond then [else])
    [(<= (length args) 3)
     (define cond-val (nl-eval (car args)))
     (set-symbol-val! sym-it cond-val)
     (if (nl-truthy? cond-val)
         (if (pair? (cdr args)) (nl-eval (cadr args)) cond-val)
         (if (and (pair? (cdr args)) (pair? (cddr args)))
             (nl-eval (caddr args))
             nl-nil))]
    ;; Multi-branch if: (if c1 e1 c2 e2 ... [default])
    [else
     (let loop ([rem args])
       (cond
         [(null? rem) nl-nil]
         [(= (length rem) 1)
          ;; Single remaining expression is default
          (nl-eval (car rem))]
         [else
          (define cond-val (nl-eval (car rem)))
          (set-symbol-val! sym-it cond-val)
          (if (nl-truthy? cond-val)
              (nl-eval (cadr rem))
              (loop (cddr rem)))]))]))

(define (eval-cond args)
  (let loop ([clauses args])
    (if (null? clauses)
        nl-nil
        (let* ([clause (car clauses)]
               [cond-expr (car clause)]
               [body (cdr clause)]
               [cond-val (nl-eval cond-expr)])
          (set-symbol-val! sym-it cond-val)
          (if (nl-truthy? cond-val)
              (if (null? body) cond-val (eval-body body))
              (loop (cdr clauses)))))))

(define (eval-case args)
  (if (null? args)
      nl-nil
      (let* ([switch-val (nl-eval (car args))]
             [clauses (cdr args)])
        (let loop ([cls clauses])
          (if (null? cls)
              nl-nil
              (let* ([clause (car cls)]
                     [key (car clause)]
                     [body (cdr clause)])
                (if (equal? switch-val key)
                    (eval-body body)
                    (loop (cdr cls)))))))))

(define (eval-define args is-macro?)
  (if (null? args)
      nl-nil
      (let ([head (car args)]
            [body (cdr args)])
        (cond
          ;; (define (name param1 ...) body...)
          [(pair? head)
           (define name-sym (car head))
           (define params (cdr head))
           (define lam (nl-lambda params body is-macro? (nl-symbol-context-name name-sym)))
           (set-symbol-val! name-sym lam)
           lam]
          ;; (define name [exp])
          [(nl-symbol? head)
           (define val (if (pair? body) (nl-eval (car body)) nl-nil))
           (set-symbol-val! head val)
           val]
          [else
           (error 'define "invalid syntax for define: ~a" head)]))))

(define (eval-macro-def args)
  ;; macro defines an expansion macro: (macro (name params...) body...)
  (eval-define args #t))

(define (eval-lambda-constructor args is-macro?)
  (if (null? args)
      (nl-lambda '() '() is-macro? (nl-context-name (current-context)))
      (nl-lambda (car args) (cdr args) is-macro? (nl-context-name (current-context)))))

(define (eval-let args is-sequential?)
  (when (null? args)
    (error 'let "missing bindings in let"))
  (define raw-bindings (car args))
  (define body (cdr args))
  (define pairs
    (cond
      [(null? raw-bindings) '()]
      ;; Nested: ((x 1) (y 2))
      [(pair? (car raw-bindings))
       (map (lambda (b)
              (if (pair? (cdr b))
                  (cons (car b) (cadr b))
                  (cons (car b) nl-nil)))
            raw-bindings)]
      ;; Flat: (x 1 y 2)
      [else
       (let flat-loop ([lst raw-bindings])
         (cond
           [(null? lst) '()]
           [(null? (cdr lst)) (list (cons (car lst) nl-nil))]
           [else (cons (cons (car lst) (cadr lst))
                       (flat-loop (cddr lst)))]))]))

  (if is-sequential?
      ;; letn: evaluate and bind sequentially
      (let seq-loop ([rem-pairs pairs] [saved-bindings '()])
        (if (null? rem-pairs)
            (eval-body body)
            (let* ([sym (caar rem-pairs)]
                   [val-expr (cdar rem-pairs)]
                   [val (nl-eval val-expr)])
              (with-dynamic-bindings (list (cons sym val))
                (lambda ()
                  (seq-loop (cdr rem-pairs) saved-bindings))))))
      ;; let: evaluate all initializers first
      (let ([evaluated-pairs
             (for/list ([p pairs])
               (cons (car p) (nl-eval (cdr p))))])
        (with-dynamic-bindings evaluated-pairs
          (lambda ()
            (eval-body body))))))

(define (eval-letex args)
  (when (null? args)
    (error 'letex "missing bindings in letex"))
  (define raw-bindings (car args))
  (define body (cdr args))
  (define pairs
    (cond
      [(null? raw-bindings) '()]
      [(pair? (car raw-bindings))
       (for/list ([b raw-bindings])
         (cons (car b) (if (pair? (cdr b)) (nl-eval (cadr b)) nl-nil)))]
      [else
       (let loop ([lst raw-bindings])
         (cond
           [(null? lst) '()]
           [(null? (cdr lst)) (list (cons (car lst) nl-nil))]
           [else (cons (cons (car lst) (nl-eval (cadr lst)))
                       (loop (cddr lst)))]))]))
  ;; Expand variables into body
  (define expanded-body
    (for/list ([expr body])
      (substitute-symbols expr pairs)))
  (eval-body expanded-body))

(define (substitute-symbols expr pairs)
  (cond
    [(nl-symbol? expr)
     (define p (assoc expr pairs))
     (if p (cdr p) expr)]
    [(pair? expr)
     (cons (substitute-symbols (car expr) pairs)
           (substitute-symbols (cdr expr) pairs))]
    [else expr]))

(define (eval-expand args)
  (when (null? args)
    (error 'expand "expected at least 1 argument"))
  (define expr (car args))
  (if (null? (cdr args))
      ;; (expand expr): expand uppercase variables bound to non-nil
      (expand-uppercase expr)
      (let ([second (cadr args)])
        (if (and (list? second) (pair? second) (pair? (car second)))
            ;; (expand list alist [bool])
            (let* ([alist (cadr args)]
                   [eval-vals? (and (pair? (cddr args)) (nl-truthy? (nl-eval (caddr args))))]
                   [pairs (map (lambda (p)
                                 (cons (car p) (if eval-vals? (nl-eval (cadr p)) (cadr p))))
                               alist)])
              (substitute-symbols expr pairs))
            ;; (expand exp sym-1 sym-2 ...)
            (let ([syms (cdr args)])
              (define pairs
                (for/list ([s syms])
                  (cons s (nl-symbol-value s))))
              (substitute-symbols expr pairs))))))

(define (expand-uppercase expr)
  (cond
    [(nl-symbol? expr)
     (define name (nl-symbol-name expr))
     (if (and (> (string-length name) 0)
              (char-upper-case? (string-ref name 0))
              (not (nl-nil? (nl-symbol-value expr))))
         (nl-symbol-value expr)
         expr)]
    [(pair? expr)
     (cons (expand-uppercase (car expr))
           (expand-uppercase (cdr expr)))]
    [else expr]))

(define (eval-while args)
  (define cond-expr (car args))
  (define body (cdr args))
  (let loop ([idx 0] [last-res nl-nil])
    (define cond-val (nl-eval cond-expr))
    (set-symbol-val! sym-it cond-val)
    (if (nl-truthy? cond-val)
        (begin
          (set-symbol-val! sym-idx idx)
          (let ([res (eval-body body)])
            (loop (+ idx 1) res)))
        last-res)))

(define (eval-until args)
  (define cond-expr (car args))
  (define body (cdr args))
  (let loop ([idx 0] [last-res nl-nil])
    (define cond-val (nl-eval cond-expr))
    (set-symbol-val! sym-it cond-val)
    (if (not (nl-truthy? cond-val))
        (begin
          (set-symbol-val! sym-idx idx)
          (let ([res (eval-body body)])
            (loop (+ idx 1) res)))
        last-res)))

(define (eval-do-while args)
  (define cond-expr (car args))
  (define body (cdr args))
  (let loop ([idx 0])
    (set-symbol-val! sym-idx idx)
    (define res (eval-body body))
    (define cond-val (nl-eval cond-expr))
    (set-symbol-val! sym-it cond-val)
    (if (nl-truthy? cond-val)
        (loop (+ idx 1))
        res)))

(define (eval-do-until args)
  (define cond-expr (car args))
  (define body (cdr args))
  (let loop ([idx 0])
    (set-symbol-val! sym-idx idx)
    (define res (eval-body body))
    (define cond-val (nl-eval cond-expr))
    (set-symbol-val! sym-it cond-val)
    (if (not (nl-truthy? cond-val))
        (loop (+ idx 1))
        res)))

(define (eval-dotimes args)
  (define spec (car args))
  (define body (cdr args))
  (define sym (car spec))
  (define count (nl-eval (cadr spec)))
  (define break-expr (if (pair? (cddr spec)) (caddr spec) #f))
  (let loop ([i 0] [last-res nl-nil])
    (if (>= i count)
        last-res
        (with-dynamic-bindings (list (cons sym i))
          (lambda ()
            (if (and break-expr (nl-truthy? (nl-eval break-expr)))
                (nl-eval break-expr)
                (let ([res (eval-body body)])
                  (loop (+ i 1) res))))))))

(define (eval-dolist args)
  (define spec (car args))
  (define body (cdr args))
  (define sym (car spec))
  (define target (nl-eval (cadr spec)))
  (define break-expr (if (pair? (cddr spec)) (caddr spec) #f))
  (define items
    (cond
      [(list? target) target]
      [(nl-array? target) (nl-array->list target)]
      [else '()]))
  (let loop ([rem items] [idx 0] [last-res nl-nil])
    (if (null? rem)
        last-res
        (with-dynamic-bindings (list (cons sym (car rem)) (cons sym-idx idx))
          (lambda ()
            (if (and break-expr (nl-truthy? (nl-eval break-expr)))
                (nl-eval break-expr)
                (let ([res (eval-body body)])
                  (loop (cdr rem) (+ idx 1) res))))))))

(define (eval-dostring args)
  (define spec (car args))
  (define body (cdr args))
  (define sym (car spec))
  (define str (nl-eval (cadr spec)))
  (define break-expr (if (pair? (cddr spec)) (caddr spec) #f))
  (define chars (string->list (if (string? str) str (~a str))))
  (let loop ([rem chars] [idx 0] [last-res nl-nil])
    (if (null? rem)
        last-res
        (with-dynamic-bindings (list (cons sym (string (car rem))) (cons sym-idx idx))
          (lambda ()
            (if (and break-expr (nl-truthy? (nl-eval break-expr)))
                (nl-eval break-expr)
                (let ([res (eval-body body)])
                  (loop (cdr rem) (+ idx 1) res))))))))

(define (eval-dotree args)
  (define spec (car args))
  (define body (cdr args))
  (define sym (car spec))
  (define ctx-target
    (if (pair? (cdr spec))
        (let ([c (nl-eval (cadr spec))])
          (if (nl-context? c) c (get-or-create-context (nl-symbol-name c))))
        (current-context)))
  (define sym-list (hash-values (nl-context-symbols ctx-target)))
  (let loop ([rem sym-list] [last-res nl-nil])
    (if (null? rem)
        last-res
        (with-dynamic-bindings (list (cons sym (car rem)))
          (lambda ()
            (let ([res (eval-body body)])
              (loop (cdr rem) res)))))))

(define (eval-for args)
  (define spec (car args))
  (define body (cdr args))
  (define sym (car spec))
  (define from (nl-eval (cadr spec)))
  (define to (nl-eval (caddr spec)))
  (define step (if (pair? (cdddr spec))
                   (nl-eval (cadddr spec))
                   (if (> to from) 1 -1)))
  (define break-expr (if (and (pair? (cdddr spec)) (pair? (cddddr spec)))
                         (car (cddddr spec))
                         #f))
  (define (continue? cur)
    (if (> step 0) (<= cur to) (>= cur to)))
  (let loop ([cur from] [last-res nl-nil])
    (if (not (continue? cur))
        last-res
        (with-dynamic-bindings (list (cons sym cur))
          (lambda ()
            (if (and break-expr (nl-truthy? (nl-eval break-expr)))
                (nl-eval break-expr)
                (let ([res (eval-body body)])
                  (loop (+ cur step) res))))))))

(define (eval-catch args)
  (define expr (car args))
  (define var-sym
    (if (pair? (cdr args))
        (let ([s (nl-eval (cadr args))])
          (if (nl-symbol? s) s (cadr args)))
        #f))
  (call-with-continuation-prompt
   (lambda ()
     (with-handlers
         ([exn:fail?
           (lambda (e)
             (if var-sym
                 (begin
                   (set-symbol-val! var-sym (string-append "ERR: " (exn-message e)))
                   nl-nil)
                 (error 'catch (exn-message e))))])
       (define res (nl-eval expr))
       (if var-sym
           (begin
             (set-symbol-val! var-sym res)
             nl-true)
           res)))
   nl-catch-prompt-tag
   (lambda (thrown)
     (define val (nl-throw-exn-val thrown))
     (if var-sym
         (begin
           (set-symbol-val! var-sym val)
           nl-true)
         val))))

(define (eval-and args)
  (let loop ([exprs args] [last-res nl-true])
    (if (null? exprs)
        last-res
        (let ([res (nl-eval (car exprs))])
          (if (nl-truthy? res)
              (loop (cdr exprs) res)
              nl-nil)))))

(define (eval-or args)
  (let loop ([exprs args])
    (if (null? exprs)
        nl-nil
        (let ([res (nl-eval (car exprs))])
          (if (nl-truthy? res)
              res
              (loop (cdr exprs)))))))

(define (eval-curry args)
  (define func-expr (car args))
  (define first-arg-expr (cadr args))
  (define sym-x (find-or-create-symbol "$x" main-context))
  (nl-lambda (list sym-x) (list (list func-expr first-arg-expr sym-x)) #f (nl-context-name (current-context))))

(define (eval-bind args)
  (when (null? args) (error 'bind "expected at least 1 argument"))
  (define alist (nl-eval (car args)))
  (define eval-vals?
    (if (pair? (cdr args))
        (nl-truthy? (nl-eval (cadr args)))
        #t))
  (unless (list? alist) (error 'bind "expected association list"))
  (for ([pair alist])
    (when (pair? pair)
      (define s (car pair))
      (define v (if (pair? (cdr pair))
                    (if eval-vals? (nl-eval (cadr pair)) (cadr pair))
                    nl-nil))
      (define sym (cond [(nl-symbol? s) s]
                        [(string? s) (find-or-create-symbol s (current-context))]
                        [else (error 'bind "invalid symbol in bind: ~a" s)]))
      (set-symbol-val! sym v)))
  nl-true)

(define (eval-constant args)
  (let loop ([rem args] [last-val nl-nil])
    (cond
      [(null? rem) last-val]
      [(null? (cdr rem)) (error 'constant "odd number of arguments to constant")]
      [else
       (define sym-expr (car rem))
       (define val (nl-eval (cadr rem)))
       (define sym
         (cond
           [(nl-symbol? sym-expr) sym-expr]
           [(string? sym-expr) (find-or-create-symbol sym-expr (current-context))]
           [else (error 'constant "invalid symbol: ~a" sym-expr)]))
       (set-nl-symbol-value! sym val)
       (set-nl-symbol-protected?! sym #t)
       (loop (cddr rem) val)])))

(define (eval-def-new args)
  (when (null? args) (error 'def-new "expected at least 1 argument"))
  (define src-expr (car args))
  (define src-ctx
    (cond
      [(nl-context? src-expr) src-expr]
      [(nl-symbol? src-expr)
       (define val (nl-symbol-value src-expr))
       (if (nl-context? val) val (get-or-create-context (nl-symbol-name src-expr)))]
      [(string? src-expr) (get-or-create-context src-expr)]
      [else (error 'def-new "invalid source context: ~a" src-expr)]))
  (define target-name
    (if (pair? (cdr args))
        (let ([tgt (cadr args)])
          (cond
            [(nl-symbol? tgt) (nl-symbol-name tgt)]
            [(string? tgt) tgt]
            [(nl-context? tgt) (nl-context-name tgt)]
            [else (~a tgt)]))
        (symbol->string (gensym "CTX_"))))
  (define target-ctx (get-or-create-context target-name))
  (when (nl-context-default-functor src-ctx)
    (set-nl-context-default-functor! target-ctx (nl-context-default-functor src-ctx)))
  (for ([(name sym) (in-hash (nl-context-symbols src-ctx))])
    (define new-sym (find-or-create-symbol name target-ctx))
    (set-nl-symbol-value! new-sym (nl-symbol-value sym)))
  (define target-sym (find-or-create-symbol target-name (current-context)))
  (set-symbol-val! target-sym target-ctx)
  target-ctx)

(define (eval-default args)
  (when (null? args) (error 'default "expected at least 1 argument"))
  (define ctx-expr (car args))
  (define ctx
    (cond
      [(nl-context? ctx-expr) ctx-expr]
      [(nl-symbol? ctx-expr)
       (define v (nl-symbol-value ctx-expr))
       (if (nl-context? v) v (get-or-create-context (nl-symbol-name ctx-expr)))]
      [else (error 'default "invalid context: ~a" ctx-expr)]))
  (if (pair? (cdr args))
      (let ([val (nl-eval (cadr args))])
        (set-nl-context-default-functor! ctx val)
        (define def-sym (find-or-create-symbol (nl-context-name ctx) ctx))
        (set-nl-symbol-value! def-sym val)
        val)
      (or (nl-context-default-functor ctx) nl-nil)))

(define (eval-doargs args)
  (define spec (car args))
  (define body (cdr args))
  (define sym (car spec))
  (define break-expr (if (pair? (cdr spec)) (cadr spec) #f))
  (define arg-list (current-call-args))
  (let loop ([rem arg-list] [idx 0] [last-res nl-nil])
    (if (null? rem)
        last-res
        (with-dynamic-bindings (list (cons sym (car rem)) (cons sym-idx idx))
          (lambda ()
            (if (and break-expr (nl-truthy? (nl-eval break-expr)))
                (nl-eval break-expr)
                (let ([res (eval-body body)])
                  (loop (cdr rem) (+ idx 1) res))))))))

(define (eval-collect args)
  (define expr (car args))
  (define max-count (if (pair? (cdr args)) (nl-eval (cadr args)) #f))
  (let loop ([count 0] [acc '()])
    (if (and max-count (>= count max-count))
        (reverse acc)
        (let ([res (nl-eval expr)])
          (if (or (nl-nil? res) (null? res))
              (reverse acc)
              (loop (+ count 1) (cons res acc)))))))

(define (eval-local args)
  (define syms-spec (car args))
  (define body (cdr args))
  (define syms
    (cond
      [(list? syms-spec) syms-spec]
      [(nl-symbol? syms-spec) (list syms-spec)]
      [else '()]))
  (define bindings (map (lambda (s) (cons s nl-nil)) syms))
  (with-dynamic-bindings bindings
    (lambda ()
      (eval-body body))))

(define (eval-global args)
  (for ([s args])
    (define sym (if (nl-symbol? s) s (find-or-create-symbol (~a s) (current-context))))
    (define main-sym (find-or-create-symbol (nl-symbol-name sym) main-context))
    (set-nl-symbol-value! sym (nl-symbol-value main-sym)))
  nl-true)
