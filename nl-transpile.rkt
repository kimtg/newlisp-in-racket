#lang racket/base

(require racket/math
         racket/list
         racket/string
         racket/format
         syntax/parse/pre
         "nl-types.rkt"
         "nl-reader.rkt"
         "nl-eval.rkt"
         "nl-macros.rkt")

(provide transpile-expr
         transpile-body
         compile-nl
         compile-nl-body
         eval-compiled
         eval-compiled-string
         resolve-symbol-in-context
         nl-namespace)

;; Anchor for dynamic compilation namespace
(define-namespace-anchor nl-anchor)
(define nl-namespace (namespace-anchor->namespace nl-anchor))

;; -------------------------------------------------------------------
;; Symbol Resolution Helper
;; -------------------------------------------------------------------

(define (resolve-symbol-in-context sym ctx)
  (if (not (string=? (nl-symbol-context-name sym) "MAIN"))
      sym
      (let* ([name (nl-symbol-name sym)]
             [ctx-syms (nl-context-symbols ctx)])
        (cond
          [(hash-has-key? ctx-syms name)
           (hash-ref ctx-syms name)]
          [(and (not (string=? (nl-context-name ctx) "MAIN"))
                (hash-has-key? (nl-context-symbols main-context) name))
           (hash-ref (nl-context-symbols main-context) name)]
          [else
           (find-or-create-symbol name ctx)]))))

;; -------------------------------------------------------------------
;; Main Transpiler Dispatch
;; -------------------------------------------------------------------

(define (transpile-expr expr [ctx (current-context)])
  (cond
    ;; 1. Literal Numbers, Strings, Arrays
    [(number? expr) expr]
    [(string? expr) expr]
    [(nl-array? expr) expr]

    ;; 2. Booleans, Nil, Empty List
    [(nl-nil? expr) #'nl-nil]
    [(nl-true? expr) #'nl-true]
    [(null? expr) #''()]
    [(boolean? expr) (if expr #'nl-true #'nl-nil)]

    ;; 3. Symbols
    [(nl-symbol? expr)
     (transpile-symbol expr ctx)]

    ;; 4. List / S-Expression (op . args)
    [(pair? expr)
     (transpile-list expr ctx)]

    [else
     #`'#,expr]))

(define (transpile-symbol sym ctx)
  (define name (nl-symbol-name sym))
  (cond
    [(string=? name "$it") #'(nl-symbol-value sym-it)]
    [(string=? name "$idx") #'(nl-symbol-value sym-idx)]
    [(string=? name "$args") #'(nl-symbol-value sym-args)]
    [(string=? name "main-args") #'(nl-symbol-value sym-main-args)]
    [(string=? name "nil") #'nl-nil]
    [(string=? name "true") #'nl-true]
    [else
     (define resolved-sym (resolve-symbol-in-context sym ctx))
     #`(nl-symbol-value #,resolved-sym)]))

;; -------------------------------------------------------------------
;; S-Expression Transpilation
;; -------------------------------------------------------------------

(define (transpile-list expr ctx)
  (define op (car expr))
  (define args (cdr expr))
  (define op-name (and (nl-symbol? op) (nl-symbol-name op)))

  (cond
    ;; Integer in operator position: Implicit Rest/Slice (1 lst) or (2 2 lst)
    [(exact-integer? op)
     #`(nl-fast-slice #,op (list #,@(for/list ([a args]) (transpile-expr a ctx))))]

    ;; quote
    [(and op-name (string=? op-name "quote"))
     #`(quote #,(if (pair? args) (car args) '()))]

    ;; begin
    [(and op-name (string=? op-name "begin"))
     #`(let () #,@(for/list ([e args]) (transpile-expr e ctx)))]

    ;; if
    [(and op-name (string=? op-name "if"))
     (transpile-if args ctx)]

    ;; when
    [(and op-name (string=? op-name "when"))
     (if (null? args)
         #'nl-nil
         #`(nl-when #,(transpile-expr (car args) ctx)
             #,@(for/list ([e (cdr args)]) (transpile-expr e ctx))))]

    ;; unless
    [(and op-name (string=? op-name "unless"))
     (if (null? args)
         #'nl-nil
         #`(nl-unless #,(transpile-expr (car args) ctx)
             #,@(for/list ([e (cdr args)]) (transpile-expr e ctx))))]

    ;; cond
    [(and op-name (string=? op-name "cond"))
     (transpile-cond args ctx)]

    ;; case
    [(and op-name (string=? op-name "case"))
     (transpile-case args ctx)]

    ;; while
    [(and op-name (string=? op-name "while"))
     (if (null? args)
         #'nl-nil
         #`(nl-while #,(transpile-expr (car args) ctx)
             #,@(for/list ([e (cdr args)]) (transpile-expr e ctx))))]

    ;; until
    [(and op-name (string=? op-name "until"))
     (if (null? args)
         #'nl-nil
         #`(nl-until #,(transpile-expr (car args) ctx)
             #,@(for/list ([e (cdr args)]) (transpile-expr e ctx))))]

    ;; dotimes
    [(and op-name (string=? op-name "dotimes"))
     (transpile-dotimes args ctx)]

    ;; dolist
    [(and op-name (string=? op-name "dolist"))
     (transpile-dolist args ctx)]

    ;; setq
    [(and op-name (string=? op-name "setq"))
     (transpile-setq args ctx)]

    ;; set
    [(and op-name (string=? op-name "set"))
     (transpile-set args ctx)]

    ;; setf
    [(and op-name (string=? op-name "setf"))
     (transpile-setf args ctx)]

    ;; push (place must be unevaluated for mutation)
    [(and op-name (string=? op-name "push"))
     #`(nl-eval (list (find-or-create-symbol "push" main-context)
                      #,(transpile-expr (car args) ctx)
                      '#,(cadr args)
                      #,@(if (pair? (cddr args)) (list (transpile-expr (caddr args) ctx)) '())))]

    ;; pop (place must be unevaluated)
    [(and op-name (string=? op-name "pop"))
     #`(nl-eval (list (find-or-create-symbol "pop" main-context)
                      '#,(car args)
                      #,@(if (pair? (cdr args)) (list (transpile-expr (cadr args) ctx)) '())))]

    ;; swap (places must be unevaluated)
    [(and op-name (string=? op-name "swap"))
     #`(nl-eval (list (find-or-create-symbol "swap" main-context)
                      '#,(car args)
                      '#,(cadr args)))]

    ;; ++
    [(and op-name (string=? op-name "++"))
     (transpile-inc-dec '++ args ctx)]

    ;; --
    [(and op-name (string=? op-name "--"))
     (transpile-inc-dec '-- args ctx)]

    ;; inc
    [(and op-name (string=? op-name "inc"))
     (transpile-inc-dec 'inc args ctx)]

    ;; dec
    [(and op-name (string=? op-name "dec"))
     (transpile-inc-dec 'dec args ctx)]

    ;; define
    [(and op-name (string=? op-name "define"))
     (transpile-define args ctx #f)]

    ;; define-macro
    [(and op-name (string=? op-name "define-macro"))
     (transpile-define args ctx #t)]

    ;; lambda / fn
    [(and op-name (or (string=? op-name "lambda") (string=? op-name "fn")))
     (transpile-lambda args ctx #f)]

    ;; let
    [(and op-name (string=? op-name "let"))
     (transpile-let args ctx #f)]

    ;; letn
    [(and op-name (string=? op-name "letn"))
     (transpile-let args ctx #t)]

    ;; catch
    [(and op-name (string=? op-name "catch"))
     (transpile-catch args ctx)]

    ;; throw
    [(and op-name (string=? op-name "throw"))
     #`(abort-current-continuation nl-catch-prompt-tag (nl-throw-exn #,(transpile-expr (car args) ctx)))]

    ;; and
    [(and op-name (string=? op-name "and"))
     (transpile-and args ctx)]

    ;; or
    [(and op-name (string=? op-name "or"))
     (transpile-or args ctx)]

    ;; silent
    [(and op-name (string=? op-name "silent"))
     #`(begin #,@(for/list ([e args]) (transpile-expr e ctx)) nl-nil)]

    ;; bind
    [(and op-name (string=? op-name "bind"))
     #`(eval-bind '#,args)]

    ;; constant
    [(and op-name (string=? op-name "constant"))
     #`(eval-constant '#,args)]

    ;; def-new
    [(and op-name (string=? op-name "def-new"))
     #`(eval-def-new '#,args)]

    ;; default
    [(and op-name (string=? op-name "default"))
     #`(eval-default '#,args)]

    ;; doargs
    [(and op-name (string=? op-name "doargs"))
     #`(eval-doargs '#,args)]

    ;; collect
    [(and op-name (string=? op-name "collect"))
     #`(eval-collect '#,args)]

    ;; local
    [(and op-name (string=? op-name "local"))
     #`(eval-local '#,args)]

    ;; global
    [(and op-name (string=? op-name "global"))
     #`(eval-global '#,args)]

    ;; for
    [(and op-name (string=? op-name "for"))
     #`(eval-for '#,args)]

    ;; dostring
    [(and op-name (string=? op-name "dostring"))
     #`(eval-dostring '#,args)]

    ;; dotree
    [(and op-name (string=? op-name "dotree"))
     #`(eval-dotree '#,args)]

    ;; do-while
    [(and op-name (string=? op-name "do-while"))
     #`(eval-do-while '#,args)]

    ;; do-until
    [(and op-name (string=? op-name "do-until"))
     #`(eval-do-until '#,args)]

    ;; curry
    [(and op-name (string=? op-name "curry"))
     #`(eval-curry '#,args)]

    ;; Fast Math (+, -, *, /, %)
    [(and op-name (string=? op-name "+"))
     (transpile-math 'nl-fast-+ 'nl-nary-+ args ctx 0)]

    [(and op-name (string=? op-name "-"))
     (transpile-math 'nl-fast-- 'nl-nary-- args ctx 0)]

    [(and op-name (string=? op-name "*"))
     (transpile-math 'nl-fast-* 'nl-nary-* args ctx 1)]

    [(and op-name (string=? op-name "/"))
     (transpile-math 'nl-fast-/ 'nl-nary-/ args ctx 1)]

    [(and op-name (string=? op-name "%"))
     #`(nl-fast-% #,(transpile-expr (car args) ctx) #,(transpile-expr (cadr args) ctx))]

    ;; Fast Comparisons (<, >, <=, >=, =, !=)
    [(and op-name (string=? op-name "<"))
     (transpile-cmp 'nl-fast-< args ctx)]
    [(and op-name (string=? op-name ">"))
     (transpile-cmp 'nl-fast-> args ctx)]
    [(and op-name (string=? op-name "<="))
     (transpile-cmp 'nl-fast-<= args ctx)]
    [(and op-name (string=? op-name ">="))
     (transpile-cmp 'nl-fast->= args ctx)]
    [(and op-name (string=? op-name "="))
     (transpile-cmp 'nl-fast-= args ctx)]
    [(and op-name (string=? op-name "!="))
     (transpile-cmp 'nl-fast-!= args ctx)]

    ;; FOOP method call: (:method target arg1 ...) or (: method target arg1 ...)
    [(or (and op-name (string-prefix? op-name ":") (not (string=? op-name ":")))
         (and op-name (string=? op-name ":")))
     (transpile-foop op args ctx)]

    ;; General Application
    [else
     (transpile-general-app op args ctx)]))

;; -------------------------------------------------------------------
;; Specific Form Transpilers
;; -------------------------------------------------------------------

(define (transpile-if args ctx)
  (cond
    [(null? args) #'nl-nil]
    [(= (length args) 1) (transpile-expr (car args) ctx)]
    [(= (length args) 2)
     #`(nl-if #,(transpile-expr (car args) ctx)
              #,(transpile-expr (cadr args) ctx))]
    [(= (length args) 3)
     #`(nl-if #,(transpile-expr (car args) ctx)
              #,(transpile-expr (cadr args) ctx)
              #,(transpile-expr (caddr args) ctx))]
    [else
     ;; Multi-branch if: (if c1 t1 c2 t2 ... [default])
     (let loop ([rem args])
       (cond
         [(null? rem) #'nl-nil]
         [(= (length rem) 1) (transpile-expr (car rem) ctx)]
         [else
          #`(let ([it #,(transpile-expr (car rem) ctx)])
              (set-nl-symbol-value! sym-it it)
              (if (nl-truthy? it)
                  #,(transpile-expr (cadr rem) ctx)
                  #,(loop (cddr rem))))]))]))

(define (transpile-cond args ctx)
  (define clauses
    (for/list ([clause args])
      (define c (transpile-expr (car clause) ctx))
      (define body (for/list ([e (cdr clause)]) (transpile-expr e ctx)))
      #`(#,c #,@body)))
  #`(nl-cond #,@clauses))

(define (transpile-case args ctx)
  (define val-expr (transpile-expr (car args) ctx))
  (define clauses
    (for/list ([clause (cdr args)])
      (define key (car clause))
      (define body (for/list ([e (cdr clause)]) (transpile-expr e ctx)))
      #`(#,key #,@body)))
  #`(nl-case #,val-expr #,@clauses))

(define (transpile-dotimes args ctx)
  (define header (car args))
  (define var-sym (resolve-symbol-in-context (car header) ctx))
  (define count-expr (transpile-expr (cadr header) ctx))
  (define break-cond
    (if (pair? (cddr header))
        (transpile-expr (caddr header) ctx)
        #f))
  (define body-exprs
    (for/list ([e (cdr args)])
      (transpile-expr e ctx)))
  (if break-cond
      #`(nl-dotimes (#,var-sym #,count-expr #,break-cond) #,@body-exprs)
      #`(nl-dotimes (#,var-sym #,count-expr) #,@body-exprs)))

(define (transpile-dolist args ctx)
  (define header (car args))
  (define var-sym (resolve-symbol-in-context (car header) ctx))
  (define list-expr (transpile-expr (cadr header) ctx))
  (define break-cond
    (if (pair? (cddr header))
        (transpile-expr (caddr header) ctx)
        #f))
  (define body-exprs
    (for/list ([e (cdr args)])
      (transpile-expr e ctx)))
  (if break-cond
      #`(nl-dolist (#,var-sym #,list-expr #,break-cond) #,@body-exprs)
      #`(nl-dolist (#,var-sym #,list-expr) #,@body-exprs)))

(define (transpile-setq args ctx)
  (let loop ([pairs args] [accum '()])
    (if (null? pairs)
        #`(begin #,@(reverse accum))
        (let ([var (car pairs)]
              [val (if (pair? (cdr pairs)) (cadr pairs) nl-nil)]
              [rest (if (pair? (cdr pairs)) (cddr pairs) '())])
          (define var-sym (resolve-symbol-in-context (if (nl-symbol? var) var (find-or-create-symbol (~a var) ctx)) ctx))
          (define step #`(nl-setq #,var-sym #,(transpile-expr val ctx)))
          (loop rest (cons step accum))))))

(define (transpile-set args ctx)
  (let loop ([pairs args] [accum '()])
    (if (null? pairs)
        #`(begin #,@(reverse accum))
        (let ([place (car pairs)]
              [val (if (pair? (cdr pairs)) (cadr pairs) nl-nil)]
              [rest (if (pair? (cdr pairs)) (cddr pairs) '())])
          ;; (set 'x 1) -> if place is (quote sym), use setq
          (define step
            (if (and (pair? place)
                     (nl-symbol? (car place))
                     (string=? (nl-symbol-name (car place)) "quote")
                     (pair? (cdr place))
                     (nl-symbol? (cadr place)))
                (let ([var-sym (resolve-symbol-in-context (cadr place) ctx)])
                  #`(nl-setq #,var-sym #,(transpile-expr val ctx)))
                #`(eval-set #,(transpile-expr place ctx) #,(transpile-expr val ctx))))
          (loop rest (cons step accum))))))

(define (transpile-setf args ctx)
  (if (null? args)
      #'nl-nil
      (let ([place (car args)]
            [val (if (pair? (cdr args)) (cadr args) nl-nil)])
        (if (nl-symbol? place)
            (let ([var-sym (resolve-symbol-in-context place ctx)])
              #`(nl-setq #,var-sym #,(transpile-expr val ctx)))
            #`(mutate-place! '#,place #,(transpile-expr val ctx))))))

(define (transpile-inc-dec op args ctx)
  (define place (car args))
  (define delta (if (pair? (cdr args)) (transpile-expr (cadr args) ctx) #f))
  (if (nl-symbol? place)
      (let ([var-sym (resolve-symbol-in-context place ctx)])
        (case op
          [(++) (if delta #`(nl-++ #,var-sym #,delta) #`(nl-++ #,var-sym))]
          [(--) (if delta #`(nl--- #,var-sym #,delta) #`(nl--- #,var-sym))]
          [(inc) (if delta #`(nl-inc #,var-sym #,delta) #`(nl-inc #,var-sym))]
          [(dec) (if delta #`(nl-dec #,var-sym #,delta) #`(nl-dec #,var-sym))]))
      #`(nl-eval (list (find-or-create-symbol (~a '#,op) main-context) '#,place #,@(if delta (list delta) '())))))

(define (transpile-define args ctx is-macro?)
  (if (null? args)
      #'nl-nil
      (let ([head (car args)]
            [body (cdr args)])
        (cond
          ;; (define (name param1 ...) body...)
          [(pair? head)
           (define name-sym (car head))
           (define params (cdr head))
           (define resolved-name-sym (resolve-symbol-in-context name-sym ctx))
           (define home-ctx-name (nl-symbol-context-name resolved-name-sym))
           (define target-ctx (get-or-create-context home-ctx-name))

           (define compiled-proc-syntax
             (compile-lambda-proc params body target-ctx is-macro?))

           #`(let* ([proc #,compiled-proc-syntax]
                    [lam (nl-lambda '#,params '#,body #,is-macro? '#,home-ctx-name)])
               (set-nl-lambda-compiled-proc! lam proc)
               (set-symbol-val! #,resolved-name-sym lam)
               lam)]

          ;; (define name [val])
          [(nl-symbol? head)
           (define resolved-head (resolve-symbol-in-context head ctx))
           (if (pair? body)
               #`(let ([v #,(transpile-expr (car body) ctx)])
                   (set-symbol-val! #,resolved-head v)
                   v)
               #`(begin
                   (set-symbol-val! #,resolved-head nl-nil)
                   nl-nil))]
          [else
           #`(eval-define '#,args #,is-macro?)]))))

(define (transpile-lambda args ctx is-macro?)
  (if (null? args)
      #'(let ([lam (nl-lambda '() '() #f "MAIN")])
          (set-nl-lambda-compiled-proc! lam (lambda (a) nl-nil))
          lam)
      (let ([params (car args)]
            [body (cdr args)]
            [home-ctx (current-context)])
        (define compiled-proc-syntax
          (compile-lambda-proc params body home-ctx is-macro?))
        #`(let* ([proc #,compiled-proc-syntax]
                 [lam (nl-lambda '#,params '#,body #,is-macro? '#,(nl-context-name home-ctx))])
            (set-nl-lambda-compiled-proc! lam proc)
            lam))))

(define (compile-lambda-proc raw-params body target-ctx is-macro?)
  ;; Filter out commas `,`
  (define params
    (filter (lambda (p) (not (and (nl-symbol? p) (string=? (nl-symbol-name p) ","))))
            raw-params))

  (define param-syms
    (for/list ([p params])
      (resolve-symbol-in-context (if (pair? p) (car p) p) target-ctx)))

  (define param-defaults
    (for/list ([p params])
      (if (pair? p)
          (transpile-expr (cadr p) target-ctx)
          #'nl-nil)))

  (define transpiled-body
    (if (null? body)
        #'nl-nil
        #`(let ()
            #,@(for/list ([e body])
                (transpile-expr e target-ctx)))))

  (cond
    ;; Zero-allocation fast-path for 1 parameter (e.g. fib)
    [(= (length params) 1)
     (define p0 (car param-syms))
     (define def0 (car param-defaults))
     #`(lambda raw-args
         (define a0 (if (pair? raw-args) (car raw-args) #,def0))
         (define rest (if (and (pair? raw-args) (pair? (cdr raw-args))) (cdr raw-args) '()))
         (define saved-p0 (nl-symbol-value #,p0))
         (define saved-args (if (null? rest) #f (current-call-args)))
         (set-nl-symbol-value! #,p0 a0)
         (if (null? rest)
             (dynamic-wind
               void
               (lambda () #,transpiled-body)
               (lambda () (set-nl-symbol-value! #,p0 saved-p0)))
             (dynamic-wind
               (lambda () (current-call-args rest))
               (lambda () #,transpiled-body)
               (lambda ()
                 (set-nl-symbol-value! #,p0 saved-p0)
                 (current-call-args saved-args)))))]

    ;; Zero-allocation fast-path for 2 parameters
    [(= (length params) 2)
     (define p0 (car param-syms))
     (define p1 (cadr param-syms))
     (define def0 (car param-defaults))
     (define def1 (cadr param-defaults))
     #`(lambda raw-args
         (define a0 (if (pair? raw-args) (car raw-args) #,def0))
         (define a1 (if (and (pair? raw-args) (pair? (cdr raw-args))) (cadr raw-args) #,def1))
         (define rest (if (and (pair? raw-args) (pair? (cdr raw-args)) (pair? (cddr raw-args))) (cddr raw-args) '()))
         (define saved-p0 (nl-symbol-value #,p0))
         (define saved-p1 (nl-symbol-value #,p1))
         (define saved-args (if (null? rest) #f (current-call-args)))
         (set-nl-symbol-value! #,p0 a0)
         (set-nl-symbol-value! #,p1 a1)
         (if (null? rest)
             (dynamic-wind
               void
               (lambda () #,transpiled-body)
               (lambda ()
                 (set-nl-symbol-value! #,p0 saved-p0)
                 (set-nl-symbol-value! #,p1 saved-p1)))
             (dynamic-wind
               (lambda () (current-call-args rest))
               (lambda () #,transpiled-body)
               (lambda ()
                 (set-nl-symbol-value! #,p0 saved-p0)
                 (set-nl-symbol-value! #,p1 saved-p1)
                 (current-call-args saved-args)))))]

    ;; General N parameters
    [else
     #`(lambda raw-args
         (define saved-ctx (current-context))
         (define saved-args (current-call-args))
         (define saved-bindings
           (for/list ([s (list #,@param-syms)])
             (cons s (nl-symbol-value s))))
         (dynamic-wind
           (lambda ()
             (current-context #,target-ctx)
             (current-call-args (if (> (length raw-args) #,(length param-syms))
                                    (drop raw-args #,(length param-syms))
                                    '()))
             #,@(for/list ([i (in-range (length param-syms))]
                           [sym param-syms]
                           [def param-defaults])
                 #`(let ([val (if (> (length raw-args) #,i)
                                  (list-ref raw-args #,i)
                                  #,def)])
                     (set-nl-symbol-value! #,sym val))))
           (lambda () #,transpiled-body)
           (lambda ()
             (for ([b saved-bindings])
               (set-nl-symbol-value! (car b) (cdr b)))
             (current-call-args saved-args)
             (current-context saved-ctx))))]))

(define (transpile-let args ctx sequential?)
  (if (null? args)
      #'nl-nil
      (let ([bindings (car args)]
            [body (cdr args)])
        (define syms (for/list ([b bindings]) (resolve-symbol-in-context (if (pair? b) (car b) b) ctx)))
        (define val-exprs
          (for/list ([b bindings])
            (if (and (pair? b) (pair? (cdr b)))
                (transpile-expr (cadr b) ctx)
                #'nl-nil)))
        (define transpiled-body
          (if (null? body)
              #'nl-nil
              #`(let () #,@(for/list ([e body]) (transpile-expr e ctx)))))

        #`(let ([vals (list #,@val-exprs)])
            (bind-and-run (list #,@syms) vals (current-context)
              (lambda () #,transpiled-body))))))

(define (transpile-catch args ctx)
  (define body-expr (car args))
  (define err-sym (if (pair? (cdr args)) (cadr args) #f))
  (if err-sym
      #`(eval-catch (list '#,body-expr '#,err-sym))
      #`(eval-catch (list '#,body-expr))))

(define (transpile-and args ctx)
  (if (null? args)
      #'nl-true
      (let loop ([rem args])
        (if (null? (cdr rem))
            (transpile-expr (car rem) ctx)
            #`(let ([v #,(transpile-expr (car rem) ctx)])
                (if (nl-truthy? v)
                    #,(loop (cdr rem))
                    nl-nil))))))

(define (transpile-or args ctx)
  (if (null? args)
      #'nl-nil
      (let loop ([rem args])
        (if (null? (cdr rem))
            (transpile-expr (car rem) ctx)
            #`(let ([v #,(transpile-expr (car rem) ctx)])
                (if (nl-truthy? v)
                    v
                    #,(loop (cdr rem))))))))

(define (transpile-math fast-binary nary args ctx default-val)
  (cond
    [(null? args) #`#,default-val]
    [(= (length args) 2)
     #`(#,fast-binary #,(transpile-expr (car args) ctx) #,(transpile-expr (cadr args) ctx))]
    [else
     #`(#,nary #,@(for/list ([a args]) (transpile-expr a ctx)))]))

(define (transpile-cmp fast-cmp args ctx)
  (if (= (length args) 2)
      #`(#,fast-cmp #,(transpile-expr (car args) ctx) #,(transpile-expr (cadr args) ctx))
      #`(nl-fast-call '#,fast-cmp (list #,@(for/list ([a args]) (transpile-expr a ctx))))))

(define (transpile-foop op args ctx)
  (define-values (method-sym target-expr rest-args)
    (if (and (nl-symbol? op) (string=? (nl-symbol-name op) ":"))
        (values (car args) (cadr args) (cddr args))
        (values op (car args) (cdr args))))
  (define method-str (if (nl-symbol? method-sym) (nl-symbol-name method-sym) (~a method-sym)))
  (define clean-name (if (string-prefix? method-str ":") (substring method-str 1) method-str))
  #`(nl-foop-dispatch '#,clean-name
                      '#,target-expr
                      #,(transpile-expr target-expr ctx)
                      (list #,@(for/list ([a rest-args]) (transpile-expr a ctx)))))

(define (transpile-general-app op args ctx)
  (if (nl-symbol? op)
      (let ([op-sym (resolve-symbol-in-context op ctx)])
        #`(let ([fn-val (nl-symbol-value #,op-sym)])
            (if (and (nl-lambda? fn-val) (nl-lambda-compiled-proc fn-val))
                ((nl-lambda-compiled-proc fn-val) #,@(for/list ([a args]) (transpile-expr a ctx)))
                (nl-fast-call fn-val (list #,@(for/list ([a args]) (transpile-expr a ctx)))))))
      #`(nl-fast-call #,(transpile-expr op ctx) (list #,@(for/list ([a args]) (transpile-expr a ctx))))))

(define (transpile-body exprs ctx)
  (if (null? exprs)
      #'nl-nil
      #`(let ()
          #,@(for/list ([e exprs])
              (transpile-expr e ctx)))))

;; -------------------------------------------------------------------
;; Compiler API
;; -------------------------------------------------------------------

(define (compile-nl expr [ctx (current-context)])
  (define stx (transpile-expr expr ctx))
  (define wrapped-stx
    #`(lambda ()
        #,stx))
  (eval (compile wrapped-stx) nl-namespace))

(define (compile-nl-body exprs [ctx (current-context)])
  (if (null? exprs)
      (lambda () nl-nil)
      (let ()
        (define transpiled-exprs
          (for/list ([e exprs])
            (transpile-expr e ctx)))
        (define wrapped-stx
          #`(lambda ()
              (let ()
                #,@transpiled-exprs)))
        (eval (compile wrapped-stx) nl-namespace))))

(define (eval-compiled expr [ctx (current-context)])
  (define proc (compile-nl expr ctx))
  (proc))

(define (eval-compiled-string code-str [ctx (current-context)])
  (define exprs
    (nl-read-all code-str (lambda (s) (find-or-create-symbol s ctx))))
  (define proc (compile-nl-body exprs ctx))
  (proc))
