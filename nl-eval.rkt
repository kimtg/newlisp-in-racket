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
;; Dynamic Scoping Mechanism & FOOP Execution State
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

;; Current FOOP target and place (fast thread-cell backed accessors)
(define *self-target-cell* (make-thread-cell nl-nil #t))
(define *self-updater-cell* (make-thread-cell void #t))

(define current-self-target
  (case-lambda
    [() (thread-cell-ref *self-target-cell*)]
    [(val) (thread-cell-set! *self-target-cell* val)]))

(define current-self-updater
  (case-lambda
    [() (thread-cell-ref *self-updater-cell*)]
    [(fn) (thread-cell-set! *self-updater-cell* fn)]))

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
;; Transpiler / Compiler Execution Hooks (Tree-walker eliminated)
;; -------------------------------------------------------------------

(define nl-eval-handler (make-parameter #f))
(define nl-eval-body-handler (make-parameter #f))
(define nl-compile-lambda-handler (make-parameter #f))

(define (nl-eval expr [ctx #f])
  (when ctx (current-context ctx))
  (define handler (nl-eval-handler))
  (if handler
      (handler expr (or ctx (current-context)))
      (error 'eval "nl-eval handler not initialized; transpile module required")))

(define (eval-body body [ctx #f])
  (when ctx (current-context ctx))
  (define handler (nl-eval-body-handler))
  (if handler
      (handler body (or ctx (current-context)))
      (error 'eval "eval-body handler not initialized; transpile module required")))

;; -------------------------------------------------------------------
;; Context Default Functors & Tree Dictionaries
;; -------------------------------------------------------------------

(define (get-context-default-functor ctx)
  (define def-functor (nl-context-default-functor ctx))
  (if (and def-functor (not (nl-nil? def-functor)))
      def-functor
      (let ([sym (hash-ref (nl-context-symbols ctx) (nl-context-name ctx) #f)])
        (if (and sym (not (nl-nil? (nl-symbol-value sym))))
            (nl-symbol-value sym)
            nl-nil))))

(define (make-tree-key-str k)
  (string-append "_" (if (string? k) k (~a k))))

(define (clean-tree-key k)
  (if (string-prefix? k "_")
      (substring k 1)
      k))

(define (apply-context-functor-evaluated ctx evaluated-args)
  (define def-functor (get-context-default-functor ctx))
  (cond
    [(and def-functor (not (nl-nil? def-functor)))
     (parameterize ([current-context ctx])
       (apply-evaluated def-functor evaluated-args))]
    [else
     (case (length evaluated-args)
       [(0)
        (for/list ([(k sym) (in-hash (nl-context-symbols ctx))]
                   #:when (not (nl-nil? (nl-symbol-value sym))))
          (list (clean-tree-key k) (nl-symbol-value sym)))]
       [(1)
        (define key-str (make-tree-key-str (car evaluated-args)))
        (define sym (hash-ref (nl-context-symbols ctx) key-str #f))
        (if sym (nl-symbol-value sym) nl-nil)]
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

;; -------------------------------------------------------------------
;; List & String Slicing / Indexing Helpers
;; -------------------------------------------------------------------

(define (nl-index-list lst indices)
  (if (null? indices)
      lst
      (if (and (= (length indices) 1) (pair? (car indices)))
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
     (nl-slice-string str (car args) (cadr args))]
    [else
     (error 'eval "invalid string indexing: ~a" args)]))

(define (apply-implicit-slice-evaluated offset-int evaluated-args)
  (cond
    [(= (length evaluated-args) 1)
     (define target (car evaluated-args))
     (cond
       [(list? target) (nl-slice-list target offset-int (- (length target) (max 0 offset-int)))]
       [(string? target) (nl-slice-string target offset-int (- (string-length target) (max 0 offset-int)))]
       [(nl-array? target)
        (make-nl-array (list (length (nl-array->list target)))
                       (nl-slice-list (nl-array->list target) offset-int (- (length (nl-array->list target)) (max 0 offset-int))))]
       [else (error 'eval "invalid target for implicit rest/slice: ~a" target)])]
    [(= (length evaluated-args) 2)
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
     (error 'eval "invalid arguments for implicit slice: ~a" evaluated-args)]))

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
;; Evaluated Functor Application (used by apply, map, filter, etc.)
;; -------------------------------------------------------------------

(define (apply-evaluated functor actual-args)
  (cond
    [(nl-primitive? functor)
     ((nl-primitive-proc functor) nl-eval actual-args (current-context))]
    [(nl-lambda? functor)
     (define proc
       (or (nl-lambda-compiled-proc functor)
           (let ([compiler (nl-compile-lambda-handler)])
             (if compiler
                 (let ([p (compiler (nl-lambda-params functor)
                                    (nl-lambda-body functor)
                                    (get-or-create-context (nl-lambda-ctx-name functor))
                                    (nl-lambda-is-macro? functor))])
                   (set-nl-lambda-compiled-proc! functor p)
                   p)
                 (error 'eval "cannot call uncompiled lambda without compiler")))))
     (apply proc actual-args)]
    [(nl-context? functor)
     (apply-context-functor-evaluated functor actual-args)]
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
     (apply-implicit-slice-evaluated functor actual-args)]
    [else
     (error 'eval "invalid function: ~a" (nl->string functor))]))

;; -------------------------------------------------------------------
;; Place Mutation Engine: set, setq, setf
;; -------------------------------------------------------------------

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
        (if (null? (cdr indices))
            (cond
              [(= norm-idx 0) (cons val (cdr lst))]
              [(and (= norm-idx 1) (pair? (cdr lst))) (cons (car lst) (cons val (cddr lst)))]
              [else (list-set lst norm-idx val)])
            (for/list ([elem lst] [i (in-naturals)])
              (if (= i norm-idx)
                  (list-set-path elem (cdr indices) val)
                  elem))))))

(define (string-set-index str idx val-str)
  (define len (string-length str))
  (define norm-idx (if (< idx 0) (+ len idx) idx))
  (define rep (if (string? val-str) val-str (~a val-str)))
  (string-append (substring str 0 norm-idx)
                 rep
                 (substring str (+ norm-idx 1))))

;; -------------------------------------------------------------------
;; Runtime Support for Metaprogramming & Dynamic Forms
;; -------------------------------------------------------------------

(define (eval-define args is-macro?)
  (if (null? args)
      nl-nil
      (let ([head (car args)]
            [body (cdr args)])
        (cond
          [(pair? head)
           (define name-sym (car head))
           (define params (cdr head))
           (define home-ctx (get-or-create-context (nl-symbol-context-name name-sym)))
           (define lam (nl-lambda params body is-macro? (nl-symbol-context-name name-sym)))
           (define compiler (nl-compile-lambda-handler))
           (when compiler
             (set-nl-lambda-compiled-proc! lam (compiler params body home-ctx is-macro?)))
           (set-symbol-val! name-sym lam)
           lam]
          [(nl-symbol? head)
           (define val (if (pair? body) (nl-eval (car body)) nl-nil))
           (set-symbol-val! head val)
           val]
          [else
           (error 'define "invalid syntax for define: ~a" head)]))))

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

(define (eval-curry args)
  (define func-expr (car args))
  (define first-arg-expr (cadr args))
  (define sym-x (find-or-create-symbol "$x" main-context))
  (define params (list sym-x))
  (define body (list (list func-expr first-arg-expr sym-x)))
  (define cur-ctx (current-context))
  (define lam (nl-lambda params body #f (nl-context-name cur-ctx)))
  (define compiler (nl-compile-lambda-handler))
  (when compiler
    (set-nl-lambda-compiled-proc! lam (compiler params body cur-ctx #f)))
  lam)

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
