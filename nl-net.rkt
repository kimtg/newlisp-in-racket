#lang racket/base

(require racket/tcp
         racket/port
         racket/string
         racket/format
         "nl-types.rkt"
         "nl-eval.rkt")

(provide (all-defined-out))

(struct nl-socket (id in-port out-port is-server? listener local-host local-port peer-host peer-port))

(define *socket-counter* 100)
(define *sockets* (make-hash)) ;; id -> nl-socket
(define *last-net-error* (make-parameter 0))
(define *current-ipv* (make-parameter 4))

(define (allocate-socket in-p out-p is-srv? listener l-h l-p p-h p-p)
  (set! *socket-counter* (+ *socket-counter* 1))
  (define id *socket-counter*)
  (define sock (nl-socket id in-p out-p is-srv? listener l-h l-p p-h p-p))
  (hash-set! *sockets* id sock)
  id)

(define (get-socket id)
  (hash-ref *sockets* id #f))

(define (nl-net-listen args)
  (define port (car args))
  (define host (if (pair? (cdr args)) (cadr args) #f))
  (define reuse? (if (and (pair? (cdr args)) (pair? (cddr args))) (nl-truthy? (caddr args)) #t))
  (with-handlers ([exn:fail? (lambda (e)
                               (*last-net-error* (exn-message e))
                               nl-nil)])
    (define listener (tcp-listen port 128 reuse? host))
    (allocate-socket #f #f #t listener (or host "0.0.0.0") port "" 0)))

(define (nl-net-connect args)
  (define host (car args))
  (define port (cadr args))
  (define timeout-ms (if (pair? (cddr args)) (caddr args) #f))
  (with-handlers ([exn:fail? (lambda (e)
                               (*last-net-error* (exn-message e))
                               nl-nil)])
    (define-values (in-p out-p) (tcp-connect host port))
    (define-values (l-h l-p p-h p-p) (tcp-addresses in-p #t))
    (allocate-socket in-p out-p #f #f l-h l-p p-h p-p)))

(define (nl-net-accept args)
  (define srv-id (car args))
  (define srv (get-socket srv-id))
  (if (and srv (nl-socket-is-server? srv) (nl-socket-listener srv))
      (with-handlers ([exn:fail? (lambda (e)
                                   (*last-net-error* (exn-message e))
                                   nl-nil)])
        (define-values (in-p out-p) (tcp-accept (nl-socket-listener srv)))
        (define-values (l-h l-p p-h p-p) (tcp-addresses in-p #t))
        (allocate-socket in-p out-p #f #f l-h l-p p-h p-p))
      nl-nil))

(define (nl-net-close args)
  (define id (car args))
  (define sock (get-socket id))
  (if sock
      (begin
        (when (nl-socket-listener sock)
          (tcp-close (nl-socket-listener sock)))
        (when (nl-socket-in-port sock)
          (close-input-port (nl-socket-in-port sock)))
        (when (nl-socket-out-port sock)
          (close-output-port (nl-socket-out-port sock)))
        (hash-remove! *sockets* id)
        nl-true)
      nl-nil))

(define (nl-net-send args)
  (define id (car args))
  (define data (cadr args))
  (define max-len (if (pair? (cddr args)) (caddr args) #f))
  (define sock (get-socket id))
  (if (and sock (nl-socket-out-port sock))
      (with-handlers ([exn:fail? (lambda (e)
                                   (*last-net-error* (exn-message e))
                                   nl-nil)])
        (define out (nl-socket-out-port sock))
        (define b (if (bytes? data) data (string->bytes/utf-8 (if (string? data) data (~a data)))))
        (define actual-b (if max-len (subbytes b 0 (min (bytes-length b) max-len)) b))
        (write-bytes actual-b out)
        (flush-output out)
        (bytes-length actual-b))
      nl-nil))

(define (nl-net-receive args)
  (define id (car args))
  (define sym-var (cadr args))
  (define max-bytes (caddr args))
  (define sock (get-socket id))
  (if (and sock (nl-socket-in-port sock))
      (with-handlers ([exn:fail? (lambda (e)
                                   (*last-net-error* (exn-message e))
                                   nl-nil)])
        (define in (nl-socket-in-port sock))
        (define b (read-bytes max-bytes in))
        (if (eof-object? b)
            nl-nil
            (let ([str (bytes->string/latin-1 b)])
              (when (nl-symbol? sym-var)
                (set-symbol-val! sym-var str))
              (bytes-length b))))
      nl-nil))

(define (nl-net-peek args)
  (define id (car args))
  (define sock (get-socket id))
  (if (and sock (nl-socket-in-port sock))
      (if (byte-ready? (nl-socket-in-port sock)) 1 0)
      0))

(define (nl-net-select args)
  (define sock-list (car args))
  (define mode (cadr args))
  (define timeout-us (if (pair? (cddr args)) (caddr args) 0))
  (filter (lambda (sid)
            (define s (get-socket sid))
            (and s
                 (cond
                   [(string-contains? mode "r")
                    (or (and (nl-socket-listener s) (tcp-accept-ready? (nl-socket-listener s)))
                        (and (nl-socket-in-port s) (byte-ready? (nl-socket-in-port s))))]
                   [(string-contains? mode "w")
                    (and (nl-socket-out-port s) #t)]
                   [else #f])))
          sock-list))

(define (nl-net-local args)
  (define id (car args))
  (define sock (get-socket id))
  (if sock
      (list (nl-socket-local-host sock) (nl-socket-local-port sock))
      nl-nil))

(define (nl-net-peer args)
  (define id (car args))
  (define sock (get-socket id))
  (if sock
      (list (nl-socket-peer-host sock) (nl-socket-peer-port sock))
      nl-nil))

(define (nl-net-lookup args)
  (define host (car args))
  ;; Returns IP for host or localhost
  (if (or (string=? host "localhost") (string=? host "127.0.0.1"))
      "127.0.0.1"
      host))

(define (nl-net-ping args)
  (define host (car args))
  (define timeout (if (pair? (cdr args)) (cadr args) 1000))
  (with-handlers ([exn:fail? (lambda (e) nl-nil)])
    (define t0 (current-inexact-milliseconds))
    (define-values (in out) (tcp-connect host 80))
    (close-input-port in)
    (close-output-port out)
    (- (current-inexact-milliseconds) t0)))

(define (nl-net-interface)
  (list "127.0.0.1"))

(define (nl-net-error)
  (*last-net-error*))

(define (nl-net-sessions)
  (hash-keys *sockets*))

(define (nl-net-service args)
  (define s (car args))
  (cond
    [(string=? s "http") 80]
    [(string=? s "https") 443]
    [(string=? s "ftp") 21]
    [(string=? s "ssh") 22]
    [(string=? s "telnet") 23]
    [(number? s)
     (case s
       [(80) "http"]
       [(443) "https"]
       [(21) "ftp"]
       [(22) "ssh"]
       [else (~a s)])]
    [else nl-nil]))

(define (nl-net-ipv args)
  (if (null? args)
      (*current-ipv*)
      (begin
        (*current-ipv* (car args))
        (*current-ipv*))))

(define (nl-net-eval args)
  (define host (car args))
  (define port (cadr args))
  (define expr (caddr args))
  (with-handlers ([exn:fail? (lambda (e)
                               (string-append "ERR: " (exn-message e)))])
    (define-values (in out) (tcp-connect host port))
    (displayln (nl->string expr #t) out)
    (flush-output out)
    (define res (read-line in 'any))
    (close-input-port in)
    (close-output-port out)
    (if (eof-object? res) nl-nil res)))

;; UDP stubs
(define (nl-net-send-to args)
  (define host (car args))
  (define port (cadr args))
  (define data (caddr args))
  (string-length (if (string? data) data (~a data))))

(define (nl-net-receive-from args)
  (define sock-id (car args))
  (define len (cadr args))
  (define sym-buf (caddr args))
  (when (nl-symbol? sym-buf)
    (set-symbol-val! sym-buf ""))
  0)

(define (nl-net-packet args)
  (car args))
