#lang racket/base

(require racket/cmdline
         racket/file
         racket/string
         racket/list
         "nl-types.rkt"
         "nl-reader.rkt"
         "nl-eval.rkt"
         "nl-builtins.rkt"
         "nl-transpile.rkt"
         "nl-repl.rkt")

(provide main)

(define (eval-smart code-str)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define exprs
                       (nl-read-all code-str (lambda (s) (find-or-create-symbol s (current-context)))))
                     (eval-body exprs))])
    (eval-compiled-string code-str)))

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (cond
    ;; No arguments: launch interactive REPL
    [(null? argv)
     (run-repl)]

    ;; -e expression: evaluate and exit
    [(and (>= (length argv) 2) (string=? (car argv) "-e"))
     (define expr-str (cadr argv))
     (define res (eval-smart expr-str))
     (displayln (nl->string res #t))]

    ;; script-file [args...]
    [else
     (define script-path (car argv))
     (define script-args (cdr argv))
     ;; Set main-args and $args
     (set-symbol-val! sym-main-args (cons script-path script-args))
     (set-symbol-val! sym-args script-args)
     (if (file-exists? script-path)
         (let ([source (file->string script-path)])
           (eval-smart source))
         (error 'main "cannot open script file: ~a" script-path))]))

(module+ main
  (main))
