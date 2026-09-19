#lang racket/base

(require racket/string
         racket/port
         racket/system
         "nl-types.rkt"
         "nl-reader.rkt"
         "nl-eval.rkt"
         "nl-builtins.rkt"
         "nl-transpile.rkt")

(provide run-repl)

(define BANNER "newLISP v.10.7.6 [Racket] on Windows\n")

;; Check if a buffer string has balanced parens and strings
(define (expression-complete? str)
  (define in (open-input-string str))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (let loop ()
      (define tok (next-token in))
      (if (eof-object? tok)
          #t
          (loop)))))

(define (parens-balanced? str)
  (let loop ([chars (string->list str)] [depth 0] [in-str #f] [escape #f] [in-brace 0])
    (cond
      [(null? chars)
       (and (= depth 0) (not in-str) (= in-brace 0))]
      [in-str
       (define c (car chars))
       (cond
         [escape (loop (cdr chars) depth in-str #f in-brace)]
         [(char=? c #\\) (loop (cdr chars) depth in-str #t in-brace)]
         [(char=? c #\") (loop (cdr chars) depth #f #f in-brace)]
         [else (loop (cdr chars) depth in-str #f in-brace)])]
      [(> in-brace 0)
       (define c (car chars))
       (cond
         [(char=? c #\{) (loop (cdr chars) depth in-str #f (+ in-brace 1))]
         [(char=? c #\}) (loop (cdr chars) depth in-str #f (- in-brace 1))]
         [else (loop (cdr chars) depth in-str #f in-brace)])]
      [else
       (define c (car chars))
       (cond
         [(char=? c #\;)
          ;; Skip line comment
          (let skip-comment ([rem (cdr chars)])
            (if (or (null? rem) (char=? (car rem) #\newline))
                (loop rem depth in-str escape in-brace)
                (skip-comment (cdr rem))))]
         [(char=? c #\") (loop (cdr chars) depth #t #f in-brace)]
         [(char=? c #\{) (loop (cdr chars) depth in-str #f (+ in-brace 1))]
         [(char=? c #\() (loop (cdr chars) (+ depth 1) in-str escape in-brace)]
         [(char=? c #\))
          (if (<= depth 0)
              #f ; unbalanced closing paren
              (loop (cdr chars) (- depth 1) in-str escape in-brace))]
         [else (loop (cdr chars) depth in-str escape in-brace)])])))

(define (run-repl)
  (display BANNER)
  (let repl-loop ()
    (display "> ")
    (flush-output)
    (define first-line (read-line (current-input-port) 'any))
    (cond
      [(eof-object? first-line)
       (newline)]
      [else
       (define trimmed (string-trim first-line))
       (cond
         [(string=? trimmed "")
          (repl-loop)]
         ;; Shell command: !cmd (no whitespace between ! and cmd)
         [(regexp-match? #px"^!\\S" trimmed)
          (define cmd (substring trimmed 1))
          (system cmd)
          (repl-loop)]
         ;; (exit) command
         [(string=? trimmed "(exit)")
          (void)]
         [else
          ;; Accumulate multi-line input if unbalanced
          (define full-input
            (let accum ([buf first-line])
              (if (parens-balanced? buf)
                  buf
                  (let ([next-line (read-line (current-input-port) 'any)])
                    (if (eof-object? next-line)
                        buf
                        (accum (string-append buf "\n" next-line)))))))
          (with-handlers ([exn:fail?
                           (lambda (e)
                             (printf "ERR: ~a\n" (exn-message e))
                             (repl-loop))])
            (define exprs
              (nl-read-all full-input
                           (lambda (s) (find-or-create-symbol s (current-context)))))
            (for ([expr exprs])
              (define res
                (with-handlers ([exn:fail? (lambda (e) (nl-eval expr))])
                  (eval-compiled expr)))
              (displayln (nl->string res #t)))
            (repl-loop))])])))
