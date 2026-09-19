#lang racket/base

(require racket/string
         racket/port
         "nl-types.rkt")

(provide (all-defined-out))

;; Token types
(struct token (type val) #:transparent)

(define (char-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)
      (char=? c #\page)))

(define (char-delimiter? c)
  (or (char-whitespace? c)
      (char=? c #\()
      (char=? c #\))
      (char=? c #\')
      (char=? c #\")
      (char=? c #\{)
      (char=? c #\})
      (char=? c #\;)
      (char=? c #\#)
      (char=? c #\,)))

;; Read a quoted string: "..."
(define (read-quoted-string in)
  (read-char in) ; consume opening quote
  (define out (open-output-string))
  (let loop ()
    (define c (read-char in))
    (cond
      [(eof-object? c)
       (error 'reader "unexpected EOF in quoted string")]
      [(char=? c #\")
       (get-output-string out)]
      [(char=? c #\\)
       (define next-c (read-char in))
       (cond
         [(eof-object? next-c)
          (error 'reader "unexpected EOF in string escape")]
         [(char=? next-c #\\) (write-char #\\ out) (loop)]
         [(char=? next-c #\") (write-char #\" out) (loop)]
         [(char=? next-c #\n)  (write-char #\newline out) (loop)]
         [(char=? next-c #\r)  (write-char #\return out) (loop)]
         [(char=? next-c #\t)  (write-char #\tab out) (loop)]
         [(char=? next-c #\b)  (write-char #\backspace out) (loop)]
         [(char=? next-c #\f)  (write-char #\page out) (loop)]
         [(char=? next-c #\x)
          ;; Hex escape: \xNN
          (define h1 (read-char in))
          (define h2 (read-char in))
          (if (and (not (eof-object? h1)) (not (eof-object? h2)))
              (let ([n (string->number (string h1 h2) 16)])
                (if n
                    (write-char (integer->char n) out)
                    (error 'reader "invalid hex escape in string: \\x~a~a" h1 h2)))
              (error 'reader "unexpected EOF in hex escape"))
          (loop)]
         [(char=? next-c #\u)
          ;; Unicode escape: \uNNNN
          (define u-str (make-string 4))
          (for ([i 4])
            (define uc (read-char in))
            (if (eof-object? uc)
                (error 'reader "unexpected EOF in unicode escape")
                (string-set! u-str i uc)))
          (define n (string->number u-str 16))
          (if n
              (write-char (integer->char n) out)
              (error 'reader "invalid unicode escape in string: \\u~a" u-str))
          (loop)]
         [(char-numeric? next-c)
          ;; Decimal ASCII escape: \nnn (up to 3 digits)
          (define d-chars (list next-c))
          (let d-loop ([count 1])
            (define p (peek-char in))
            (if (and (< count 3) (not (eof-object? p)) (char-numeric? p))
                (begin
                  (set! d-chars (append d-chars (list (read-char in))))
                  (d-loop (+ count 1)))
                (void)))
          (define code (string->number (list->string d-chars) 10))
          (if (and code (<= code 255))
              (write-char (integer->char code) out)
              (error 'reader "invalid decimal ASCII escape in string: \\~a" (list->string d-chars)))
          (loop)]
         [else
          (write-char next-c out)
          (loop)])]
      [else
       (write-char c out)
       (loop)])))

;; Read curly brace string: { ... }
;; Handles balanced curly braces without escape translation
(define (read-brace-string in)
  (read-char in) ; consume opening brace
  (define out (open-output-string))
  (let loop ([depth 1])
    (define c (read-char in))
    (cond
      [(eof-object? c)
       (error 'reader "unexpected EOF in curly brace string")]
      [(char=? c #\{)
       (write-char c out)
       (loop (+ depth 1))]
      [(char=? c #\})
       (if (= depth 1)
           (get-output-string out)
           (begin
             (write-char c out)
             (loop (- depth 1))))]
      [else
       (write-char c out)
       (loop depth)])))

;; Read bracketed construct: either [text]...[/text] or [bracketed symbol]
(define (read-bracketed in)
  (read-char in) ; consume opening '['
  ;; Check if next characters are "text]"
  (define tag-chars (make-string 5))
  (define count 0)
  (let read-tag ()
    (if (< count 5)
        (let ([c (peek-char in count)])
          (if (eof-object? c)
              (void)
              (begin
                (string-set! tag-chars count c)
                (set! count (+ count 1))
                (read-tag))))
        (void)))
  (if (and (= count 5) (string=? tag-chars "text]"))
      ;; Consume "text]"
      (begin
        (for ([_ 5]) (read-char in))
        ;; Read raw string until [/text]
        (let ([out (open-output-string)])
          (let loop ()
            (define c (read-char in))
            (cond
              [(eof-object? c)
               (error 'reader "unexpected EOF in [text]...[/text] string")]
              [(char=? c #\[)
               ;; Check if followed by "/text]"
               (define close-chars (make-string 6))
               (define c-count 0)
               (let check-close ()
                 (if (< c-count 6)
                     (let ([pc (peek-char in c-count)])
                       (if (eof-object? pc)
                           (void)
                           (begin
                             (string-set! close-chars c-count pc)
                             (set! c-count (+ c-count 1))
                             (check-close))))
                     (void)))
               (if (and (= c-count 6) (string=? close-chars "/text]"))
                   (begin
                     ;; Consume "/text]"
                     (for ([_ 6]) (read-char in))
                     (get-output-string out))
                   (begin
                     (write-char c out)
                     (loop)))]
              [else
               (write-char c out)
               (loop)]))))

      ;; Otherwise it's a bracketed symbol: [symbol with spaces etc.]
      (let ([out (open-output-string)])
        (let loop ()
          (define c (read-char in))
          (cond
            [(eof-object? c)
             (error 'reader "unexpected EOF in bracketed symbol")]
            [(char=? c #\])
             (token 'symbol (get-output-string out))]
            [else
             (write-char c out)
             (loop)])))))

;; Parse raw word into number, boolean, or symbol
(define (parse-word str)
  (cond
    [(string=? str "nil") nl-nil]
    [(string=? str "true") nl-true]
    ;; Hexadecimal integer: 0x... or -0x... or +0x...
    [(regexp-match-exact? #rx"^[+-]?0[xX][0-9a-fA-F]+$" str)
     (string->number (string-replace (string-replace str "0x" "#x") "0X" "#x"))]
    ;; Binary integer: 0b... or -0b... or +0b...
    [(regexp-match-exact? #rx"^[+-]?0[bB][01]+$" str)
     (string->number (string-replace (string-replace str "0b" "#b") "0B" "#b"))]
    ;; Octal integer: 0... (starts with 0, followed by octal digits)
    [(regexp-match-exact? #rx"^[+-]?0[0-7]+$" str)
     (string->number (string-replace str "0" "#o" #:all? #f))]
    ;; Explicit BigInt: ends with L or l
    [(regexp-match-exact? #rx"^[+-]?[0-9]+[Ll]$" str)
     (string->number (substring str 0 (- (string-length str) 1)))]
    ;; Standard Float / Scientific notation
    [(regexp-match-exact? #rx"^[+-]?[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?$" str)
     (string->number str)]
    [(regexp-match-exact? #rx"^[+-]?[0-9]+[eE][+-]?[0-9]+$" str)
     (string->number str)]
    ;; Standard Integer (arbitrary precision supported)
    [(regexp-match-exact? #rx"^[+-]?[0-9]+$" str)
     (string->number str)]
    ;; Otherwise, it's a symbol
    [else (token 'symbol str)]))

;; Main Lexer: next-token
(define (next-token in)
  (let skip-whitespace ()
    (define c (peek-char in))
    (cond
      [(eof-object? c) eof]
      [(char-whitespace? c)
       (read-char in)
       (skip-whitespace)]
      ;; Semicolon or Hash comment: skip to newline
      [(or (char=? c #\;) (char=? c #\#))
       (read-char in)
       (let skip-comment ()
         (define cc (read-char in))
         (unless (or (eof-object? cc) (char=? cc #\newline))
           (skip-comment)))
       (skip-whitespace)]
      [(char=? c #\()
       (read-char in)
       (token 'lparen "(")]
      [(char=? c #\))
       (read-char in)
       (token 'rparen ")")]
      [(char=? c #\')
       (read-char in)
       (token 'quote "'")]
      [(char=? c #\,)
       (read-char in)
       (token 'symbol ",")]
      [(char=? c #\")
       (token 'string (read-quoted-string in))]
      [(char=? c #\{)
       (token 'string (read-brace-string in))]
      [(char=? c #\[)
       (define res (read-bracketed in))
       (if (string? res)
           (token 'string res)
           res)]
      [(char=? c #\:)
       ;; Colon operator or FOOP method prefix: :method
       (read-char in)
       (define next-c (peek-char in))
       (if (or (eof-object? next-c) (char-delimiter? next-c))
           (token 'symbol ":")
           ;; Colon followed by symbol without space: e.g. :area
           ;; Emit ':' and leave remainder in stream
           (token 'symbol ":"))]
      [else
       ;; Read word until delimiter
       (define out (open-output-string))
       (let read-word ()
         (define wc (peek-char in))
         (if (or (eof-object? wc) (char-delimiter? wc))
             (void)
             (begin
               (write-char (read-char in) out)
               (read-word))))
       (define word (get-output-string out))
       (define parsed (parse-word word))
       (if (token? parsed)
           parsed
           (token 'literal parsed))])))

;; -------------------------------------------------------------------
;; Parser: Converts tokens to S-expressions / newLISP AST
;; -------------------------------------------------------------------

(define (nl-parse-datum in)
  (define tok (next-token in))
  (cond
    [(eof-object? tok) eof]
    [(eq? (token-type tok) 'lparen)
     ;; Read list until rparen
     (let read-list ()
       (define p (peek-token in))
       (cond
         [(eof-object? p)
          (error 'reader "unexpected EOF while reading list")]
         [(eq? (token-type p) 'rparen)
          (next-token in) ; consume ')'
          '()]
         [else
          (define elem (nl-parse-datum in))
          (cons elem (read-list))]))]
    [(eq? (token-type tok) 'rparen)
     (error 'reader "unexpected closing parenthesis ')'")]
    [(eq? (token-type tok) 'quote)
     (list (token 'symbol "quote") (nl-parse-datum in))]
    [(eq? (token-type tok) 'literal)
     (token-val tok)]
    [(eq? (token-type tok) 'string)
     (token-val tok)]
    [(eq? (token-type tok) 'symbol)
     tok]
    [else
     (error 'reader "unknown token: ~a" tok)]))

;; Helper to peek at next token without consuming it permanently
(define peeked-token #f)
(define (peek-token in)
  (if peeked-token
      peeked-token
      (let ([tok (next-token in)])
        (set! peeked-token tok)
        tok)))

;; Redefine next-token to respect peeked-token
(define raw-next-token next-token)
(set! next-token
      (lambda (in)
        (if peeked-token
            (begin0 peeked-token (set! peeked-token #f))
            (raw-next-token in))))

;; Convert parsed datum containing token 'symbol into resolved AST
(define (resolve-symbols datum context-resolver)
  (cond
    [(null? datum) '()]
    [(token? datum)
     (if (eq? (token-type datum) 'symbol)
         (context-resolver (token-val datum))
         (token-val datum))]
    [(pair? datum)
     (cons (resolve-symbols (car datum) context-resolver)
           (resolve-symbols (cdr datum) context-resolver))]
    [else datum]))

;; High-level read procedures
(define (nl-read [port (current-input-port)] [resolver values])
  (define raw (nl-parse-datum port))
  (if (eof-object? raw)
      eof
      (resolve-symbols raw resolver)))

(define (nl-read-all [port-or-str (current-input-port)] [resolver values])
  (define port (if (string? port-or-str)
                   (open-input-string port-or-str)
                   port-or-str))
  (let loop ()
    (define expr (nl-read port resolver))
    (if (eof-object? expr)
        '()
        (cons expr (loop)))))

(define (nl-read-expr str [resolver values])
  (define port (open-input-string str))
  (nl-read port resolver))
