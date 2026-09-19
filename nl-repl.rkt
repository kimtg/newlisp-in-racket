#lang racket/base

(require racket/string
         racket/list
         racket/port
         racket/system
         racket/date
         racket/file
         "nl-types.rkt"
         "nl-reader.rkt"
         "nl-eval.rkt"
         "nl-builtins.rkt"
         "nl-transpile.rkt"
         "nl-ext.rkt")

(provide (all-defined-out))

(define (get-banner)
  (define os-str
    (case (system-type 'os)
      [(windows) "Windows"]
      [(macosx) "OSX"]
      [else "Linux"]))
  (format "newLISP v.10.7.6 [Racket] on ~a\n" os-str))

(define (log-write file-path str)
  (when file-path
    (with-handlers ([exn:fail? void])
      (call-with-output-file file-path
        (lambda (out)
          (display str out)
          (flush-output out))
        #:exists 'append))))

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
  (let loop ([chars (string->list str)] [depth 0] [in-str #f] [escape #f] [in-brace 0] [in-raw-text #f])
    (cond
      [(null? chars)
       (and (= depth 0) (not in-str) (= in-brace 0) (not in-raw-text))]
      [in-raw-text
       ;; Look for [/text]
       (if (and (>= (length chars) 7)
                (string=? (list->string (take chars 7)) "[/text]"))
           (loop (drop chars 7) depth in-str escape in-brace #f)
           (loop (cdr chars) depth in-str escape in-brace #t))]
      [in-str
       (define c (car chars))
       (cond
         [escape (loop (cdr chars) depth in-str #f in-brace #f)]
         [(char=? c #\\) (loop (cdr chars) depth in-str #t in-brace #f)]
         [(char=? c #\") (loop (cdr chars) depth #f #f in-brace #f)]
         [else (loop (cdr chars) depth in-str #f in-brace #f)])]
      [(> in-brace 0)
       (define c (car chars))
       (cond
         [(char=? c #\{) (loop (cdr chars) depth in-str #f (+ in-brace 1) #f)]
         [(char=? c #\}) (loop (cdr chars) depth in-str #f (- in-brace 1) #f)]
         [else (loop (cdr chars) depth in-str #f in-brace #f)])]
      [else
       (define c (car chars))
       (cond
         [(and (char=? c #\[)
               (>= (length chars) 6)
               (string=? (list->string (take chars 6)) "[text]"))
          (loop (drop chars 6) depth in-str escape in-brace #t)]
         [(char=? c #\;)
          ;; Skip line comment
          (let skip-comment ([rem (cdr chars)])
            (if (or (null? rem) (char=? (car rem) #\newline))
                (loop rem depth in-str escape in-brace #f)
                (skip-comment (cdr rem))))]
         [(char=? c #\") (loop (cdr chars) depth #t #f in-brace #f)]
         [(char=? c #\{) (loop (cdr chars) depth in-str #f (+ in-brace 1) #f)]
         [(char=? c #\() (loop (cdr chars) (+ depth 1) in-str escape in-brace #f)]
         [(char=? c #\))
          (if (<= depth 0)
              #f ; unbalanced closing paren
              (loop (cdr chars) (- depth 1) in-str escape in-brace #f))]
         [else (loop (cdr chars) depth in-str escape in-brace #f)])])))

(define (evaluate-and-print raw-input in out log-file log-all? continue-k)
  (define input-str
    (if (*command-event-handler*)
        (with-handlers ([exn:fail? (lambda (e) raw-input)])
          (define translated (nl-eval (list (*command-event-handler*) raw-input)))
          (cond
            [(string? translated) translated]
            [(or (nl-nil? translated) (not (nl-truthy? translated))) #f]
            [else raw-input]))
        raw-input))
  (if (not input-str)
      (continue-k)
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (define err-msg (format "ERR: ~a\n" (exn-message e)))
                         (display err-msg out)
                         (flush-output out)
                         (when log-all?
                           (log-write log-file (format "[~a] OUT: ~a" (date->string (current-date) #t) err-msg)))
                         (continue-k))])
        (define exprs
          (nl-read-all input-str
                       (lambda (s) (find-or-create-symbol s (current-context)))))
        (for ([expr exprs])
          (define res
            (with-handlers ([exn:fail? (lambda (e) (nl-eval expr))])
              (eval-compiled expr)))
          (define res-str (nl->string res #t))
          (displayln res-str out)
          (flush-output out)
          (when log-all?
            (log-write log-file (format "[~a] OUT: ~a\n" (date->string (current-date) #t) res-str))))
        (continue-k))))

(define (run-repl #:prompt? [prompt? #t]
                  #:banner? [banner? #t]
                  #:in [in (current-input-port)]
                  #:out [out (current-output-port)]
                  #:log-file [log-file #f]
                  #:log-all? [log-all? #f]
                  #:exit-on-close? [exit-on-close? #t])
  (when banner?
    (display (get-banner) out)
    (flush-output out))
  (let repl-loop ()
    (when prompt?
      (define p-str
        (if (*prompt-event-handler*)
            (with-handlers ([exn:fail? (lambda (e) "> ")])
              (define r (nl-eval (list (*prompt-event-handler*))))
              (if (string? r) r "> "))
            "> "))
      (display p-str out)
      (flush-output out))
    (define first-line (read-line in 'any))
    (cond
      [(eof-object? first-line)
       (when prompt? (newline out))
       (flush-output out)
       (if exit-on-close? (void) (void))]
      [else
       (define trimmed (string-trim first-line))
       (cond
         [(string=? trimmed "")
          (repl-loop)]
         ;; [cmd] ... [/cmd] multiline bracket
         [(string=? trimmed "[cmd]")
          (define full-input
            (let accum ([lines '()])
              (define l (read-line in 'any))
              (cond
                [(or (eof-object? l) (string=? (string-trim l) "[/cmd]"))
                 (string-join (reverse lines) "\n")]
                [else (accum (cons l lines))])))
          (when log-file
            (log-write log-file (format "[~a] IN: [cmd]\n~a\n[/cmd]\n" (date->string (current-date) #t) full-input)))
          (evaluate-and-print full-input in out log-file log-all? repl-loop)]
         ;; Shell command: !cmd (no whitespace between ! and cmd)
         [(regexp-match? #px"^!\\S" trimmed)
          (define cmd (substring trimmed 1))
          (when log-file
            (log-write log-file (format "[~a] IN: !~a\n" (date->string (current-date) #t) cmd)))
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
                  (let ([next-line (read-line in 'any)])
                    (if (eof-object? next-line)
                        buf
                        (accum (string-append buf "\n" next-line)))))))
          (when log-file
            (log-write log-file (format "[~a] IN: ~a\n" (date->string (current-date) #t) full-input)))
          (evaluate-and-print full-input in out log-file log-all? repl-loop)])])))
