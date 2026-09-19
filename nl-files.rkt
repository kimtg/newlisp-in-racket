#lang racket/base

(require racket/file
         racket/port
         racket/string
         racket/format
         "nl-types.rkt"
         "nl-eval.rkt")

(provide (all-defined-out))

(struct nl-file-handle (id in-port out-port mode path [pos #:mutable]))

(define *file-handle-counter* 0)
(define *file-handles* (make-hash)) ;; id -> nl-file-handle
(define *current-load-line* (make-parameter 1))

(define (allocate-file-handle in-p out-p mode path)
  (set! *file-handle-counter* (+ *file-handle-counter* 1))
  (define id *file-handle-counter*)
  (define fh (nl-file-handle id in-p out-p mode path 0))
  (hash-set! *file-handles* id fh)
  id)

(define (get-file-handle id)
  (hash-ref *file-handles* id #f))

(define (nl-open args)
  (define path (car args))
  (define mode (if (pair? (cdr args)) (cadr args) "read"))
  (with-handlers ([exn:fail? (lambda (e) nl-nil)])
    (cond
      [(or (string=? mode "read") (string=? mode "r"))
       (define in-p (open-input-file path #:mode 'binary))
       (allocate-file-handle in-p #f "read" path)]
      [(or (string=? mode "write") (string=? mode "w"))
       (define out-p (open-output-file path #:mode 'binary #:exists 'truncate/replace))
       (allocate-file-handle #f out-p "write" path)]
      [(or (string=? mode "append") (string=? mode "a"))
       (define out-p (open-output-file path #:mode 'binary #:exists 'append))
       (allocate-file-handle #f out-p "append" path)]
      [(or (string=? mode "update") (string=? mode "u") (string=? mode "write-append") (string=? mode "wa"))
       (define in-p (open-input-file path #:mode 'binary))
       (define out-p (open-output-file path #:mode 'binary #:exists 'update))
       (allocate-file-handle in-p out-p "update" path)]
      [else nl-nil])))

(define (nl-close args)
  (define id (car args))
  (define fh (get-file-handle id))
  (if fh
      (begin
        (when (nl-file-handle-in-port fh)
          (close-input-port (nl-file-handle-in-port fh)))
        (when (nl-file-handle-out-port fh)
          (close-output-port (nl-file-handle-out-port fh)))
        (hash-remove! *file-handles* id)
        nl-true)
      nl-nil))

(define (nl-file-read args)
  (define id (car args))
  (define sym-var (cadr args))
  (define num-bytes (caddr args))
  (define fh (get-file-handle id))
  (if (and fh (nl-file-handle-in-port fh))
      (let* ([p (nl-file-handle-in-port fh)]
             [b (read-bytes num-bytes p)])
        (if (eof-object? b)
            nl-nil
            (let ([str (bytes->string/latin-1 b)])
              (when (nl-symbol? sym-var)
                (set-symbol-val! sym-var str))
              (bytes-length b))))
      nl-nil))

(define (nl-write args)
  (define id (car args))
  (define buf (cadr args))
  (define bytes-to-write (if (pair? (cddr args)) (caddr args) #f))
  (define fh (get-file-handle id))
  (if (and fh (nl-file-handle-out-port fh))
      (let* ([out (nl-file-handle-out-port fh)]
             [b (if (bytes? buf) buf (string->bytes/utf-8 (if (string? buf) buf (~a buf))))]
             [actual-b (if bytes-to-write (subbytes b 0 (min (bytes-length b) bytes-to-write)) b)])
        (write-bytes actual-b out)
        (flush-output out)
        (bytes-length actual-b))
      nl-nil))

(define (nl-read-line args)
  (define p
    (if (pair? args)
        (let ([fh (get-file-handle (car args))])
          (and fh (nl-file-handle-in-port fh)))
        (current-input-port)))
  (if p
      (let ([line (read-line p 'any)])
        (if (eof-object? line) nl-nil line))
      nl-nil))

(define (nl-write-line args)
  (define-values (p str)
    (if (= (length args) 1)
        (values (current-output-port) (car args))
        (let ([fh (get-file-handle (car args))])
          (values (and fh (nl-file-handle-out-port fh)) (cadr args)))))
  (if p
      (let ([s (if (string? str) str (~a str))])
        (display s p)
        (newline p)
        (flush-output p)
        s)
      nl-nil))

(define (nl-read-char args)
  (define p
    (if (pair? args)
        (let ([fh (get-file-handle (car args))])
          (and fh (nl-file-handle-in-port fh)))
        (current-input-port)))
  (if p
      (let ([ch (read-byte p)])
        (if (eof-object? ch) nl-nil ch))
      nl-nil))

(define (nl-write-char args)
  (define-values (p ch-code)
    (if (= (length args) 1)
        (values (current-output-port) (car args))
        (let ([fh (get-file-handle (car args))])
          (values (and fh (nl-file-handle-out-port fh)) (cadr args)))))
  (if p
      (begin
        (write-byte (modulo (if (number? ch-code) ch-code (char->integer (string-ref (~a ch-code) 0))) 256) p)
        (flush-output p)
        ch-code)
      nl-nil))

(define (nl-read-utf8 args)
  (define p
    (if (pair? args)
        (let ([fh (get-file-handle (car args))])
          (and fh (nl-file-handle-in-port fh)))
        (current-input-port)))
  (if p
      (let ([ch (read-char p)])
        (if (eof-object? ch) nl-nil (string ch)))
      nl-nil))

(define (nl-read-key)
  ;; Read a single char without echo if possible
  (let ([ch (read-char (current-input-port))])
    (if (eof-object? ch) nl-nil (char->integer ch))))

(define (nl-seek args)
  (define id (car args))
  (define fh (get-file-handle id))
  (if fh
      (let ([p (or (nl-file-handle-in-port fh) (nl-file-handle-out-port fh))])
        (if (pair? (cdr args))
            (let* ([offset (cadr args)]
                   [mode (if (pair? (cddr args)) (caddr args) 0)]) ;; 0: begin, 1: curr, 2: end
              (file-position p offset)
              (file-position p))
            (file-position p)))
      nl-nil))

(define (nl-peek args)
  (define id (car args))
  (define fh (get-file-handle id))
  (if (and fh (nl-file-handle-in-port fh))
      (let ([b (peek-byte (nl-file-handle-in-port fh))])
        (if (eof-object? b) nl-nil b))
      nl-nil))

(define (nl-device args)
  (define id (if (pair? args) (car args) 0))
  (if (= id 0)
      (terminal-port? (current-input-port))
      #f))

(define (nl-current-line)
  (*current-load-line*))

(define (nl-save args)
  (define path (car args))
  (define contexts-to-save
    (if (pair? (cdr args))
        (map (lambda (c)
               (cond
                 [(nl-context? c) c]
                 [(nl-symbol? c) (get-or-create-context (nl-symbol-name c))]
                 [else (get-or-create-context (~a c))]))
             (cdr args))
        (list main-context)))
  (with-handlers ([exn:fail? (lambda (e) nl-nil)])
    (call-with-output-file path
      (lambda (out)
        (for ([ctx contexts-to-save])
          (fprintf out ";; Context ~a\n" (nl-context-name ctx))
          (for ([(sym-name sym) (in-hash (nl-context-symbols ctx))])
            (define val (nl-symbol-value sym))
            (unless (or (nl-nil? val) (nl-primitive? val))
              (fprintf out "(setq ~a ~a)\n"
                       (nl-symbol->display-string sym (nl-context-name ctx))
                       (nl->string val #t))))))
      #:exists 'truncate/replace)
    nl-true))

(define (nl-remove-dir args)
  (define path (car args))
  (with-handlers ([exn:fail? (lambda (e) nl-nil)])
    (delete-directory path)
    nl-true))
