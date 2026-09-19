#lang racket/base

(require racket/port
         racket/path
         racket/runtime-path)

(provide read
         read-syntax
         get-info)

(define-runtime-path main-path "../main.rkt")

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) #f]
      [else default])))

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax src in)
  (define name
    (cond
      [(path? src)
       (string->symbol (path->string (path-replace-extension (file-name-from-path src) #"")))]
      [(symbol? src) src]
      [(string? src) (string->symbol src)]
      [else 'newlisp-module]))
  (define content (port->string in))
  (define main-str (path->string (simplify-path main-path)))
  (with-syntax ([main-file main-str]
                [src-val (if (path? src) (path->string src) (format "~a" src))]
                [body-str content]
                [mod-name (datum->syntax #f name)])
    #'(module mod-name racket/base
        (require (file main-file))
        (provide (all-defined-out))
        (define raw-argv (vector->list (current-command-line-arguments)))
        (define prog-name (if (string=? src-val "") "newlisp" src-val))
        (set-nl-symbol-value! sym-dollar-main-args (cons prog-name raw-argv))
        (void (eval-smart body-str)))))
