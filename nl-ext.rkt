#lang racket/base

(require racket/string
         racket/list
         racket/format
         racket/math
         racket/system
         racket/date
         racket/port
         racket/os
         "nl-types.rkt"
         "nl-eval.rkt"
         "nl-files.rkt")

(provide (all-defined-out))

;; Global parameters
(define *last-error-msg* (make-parameter "no error"))
(define *last-regex-matches* (make-parameter '()))
(define *current-locale* (make-parameter "C"))
(define *error-event-handler* (make-parameter #f))
(define *command-event-handler* (make-parameter #f))
(define *prompt-event-handler* (make-parameter #f))
(define *reader-event-handler* (make-parameter #f))

;; -------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------

(define (nl-null? args)
  (define v (car args))
  (racket->nl-bool (or (nl-nil? v) (null? v))))

(define (nl-quote? args)
  (define v (car args))
  (racket->nl-bool (and (pair? v)
                       (or (equal? (car v) 'quote)
                           (and (nl-symbol? (car v))
                                (string=? (nl-symbol-name (car v)) "quote"))))))

(define (nl-legal? args)
  (define str (car args))
  (if (string? str)
      (let ([len (string-length str)])
        (if (= len 0)
            nl-nil
            (let ([first-char (string-ref str 0)])
              (if (or (char-alphabetic? first-char)
                      (member first-char '(#\_ #\+ #\- #\* #\/ #\% #\< #\> #\= #\! #\? #\: #\$ #\& #\| #\^ #\~)))
                  nl-true
                  nl-nil))))
      nl-nil))

(define (nl-bigint? args)
  (define v (car args))
  (racket->nl-bool (exact-integer? v)))

;; -------------------------------------------------------------------
;; Type Conversions & Strings
;; -------------------------------------------------------------------

(define (nl-int args)
  (define v (car args))
  (define def (if (pair? (cdr args)) (cadr args) 0))
  (define base (if (and (pair? (cdr args)) (pair? (cddr args))) (caddr args) 10))
  (cond
    [(integer? v) v]
    [(number? v) (exact-round v)]
    [(nl-true? v) 1]
    [(nl-nil? v) 0]
    [(string? v)
     (or (string->number (string-trim v) base) def)]
    [else def]))

(define (nl-float args)
  (define v (car args))
  (define def (if (pair? (cdr args)) (cadr args) 0.0))
  (cond
    [(number? v) (exact->inexact v)]
    [(string? v) (or (string->number (string-trim v)) def)]
    [(nl-true? v) 1.0]
    [(nl-nil? v) 0.0]
    [else def]))

(define (nl-bigint args)
  (define v (car args))
  (cond
    [(exact-integer? v) v]
    [(number? v) (exact-round v)]
    [(string? v) (or (string->number (string-trim v)) 0)]
    [else 0]))

(define (nl-name args)
  (define v (car args))
  (cond
    [(nl-symbol? v) (nl-symbol-name v)]
    [(nl-context? v) (nl-context-name v)]
    [(string? v) v]
    [else (~a v)]))

(define (nl-prefix args)
  (define v (car args))
  (cond
    [(nl-symbol? v) (nl-symbol-context-name v)]
    [(nl-context? v) (nl-context-name v)]
    [else "MAIN"]))

(define (nl-address args)
  (define v (car args))
  (abs (equal-hash-code v)))

(define (nl-unicode args)
  (define v (car args))
  (cond
    [(string? v)
     (define idx (if (pair? (cdr args)) (cadr args) 0))
     (define len (string-length v))
     (define norm-idx (if (< idx 0) (+ len idx) idx))
     (if (or (< norm-idx 0) (>= norm-idx len))
         nl-nil
         (char->integer (string-ref v norm-idx)))]
    [(number? v)
     (string (integer->char (modulo v #x10FFFF)))]
    [else nl-nil]))

(define (nl-utf8 args)
  (define code (car args))
  (if (number? code)
      (string (integer->char (modulo code #x10FFFF)))
      ""))

(define (nl-utf8len args)
  (define str (car args))
  (if (string? str)
      (string-length str)
      0))

;; CRC32
(define crc-table
  (for/vector ([i (in-range 256)])
    (for/fold ([c i]) ([j (in-range 8)])
      (if (odd? c)
          (bitwise-xor (arithmetic-shift c -1) #xEDB88320)
          (arithmetic-shift c -1)))))

(define (nl-crc32 args)
  (define data (car args))
  (define bs (if (bytes? data) data (string->bytes/utf-8 (if (string? data) data (~a data)))))
  (define crc #xFFFFFFFF)
  (for ([b bs])
    (define idx (bitwise-and (bitwise-xor crc b) #xFF))
    (set! crc (bitwise-xor (arithmetic-shift crc -8) (vector-ref crc-table idx))))
  (bitwise-xor crc #xFFFFFFFF))

;; Stream Cipher Encrypt
(define (nl-encrypt args)
  (define str (car args))
  (define pad (cadr args))
  (define src-bs (string->bytes/utf-8 (if (string? str) str (~a str))))
  (define pad-bs (string->bytes/utf-8 (if (string? pad) pad (~a pad))))
  (define pad-len (max 1 (bytes-length pad-bs)))
  (define out-bs
    (list->bytes
     (for/list ([b src-bs] [i (in-naturals)])
       (bitwise-xor b (bytes-ref pad-bs (modulo i pad-len))))))
  (bytes->string/latin-1 out-bs))

(define (nl-regex-comp args)
  (car args))

;; Binary Packing and Unpacking
(define (nl-pack args)
  (define fmt (car args))
  (define vals (cdr args))
  (define out (open-output-bytes))
  (define val-idx 0)
  (define big-endian? #f)
  (for ([ch (string->list fmt)])
    (case ch
      [(#\>) (set! big-endian? #t)]
      [(#\<) (set! big-endian? #f)]
      [(#\b #\c)
       (when (< val-idx (length vals))
         (write-byte (bitwise-and (list-ref vals val-idx) #xFF) out)
         (set! val-idx (+ val-idx 1)))]
      [(#\s #\u)
       (when (< val-idx (length vals))
         (define v (list-ref vals val-idx))
         (define b1 (bitwise-and v #xFF))
         (define b2 (bitwise-and (arithmetic-shift v -8) #xFF))
         (if big-endian?
             (begin (write-byte b2 out) (write-byte b1 out))
             (begin (write-byte b1 out) (write-byte b2 out)))
         (set! val-idx (+ val-idx 1)))]
      [(#\d #\l)
       (when (< val-idx (length vals))
         (define v (list-ref vals val-idx))
         (define bs
           (for/list ([shift '(0 8 16 24)])
             (bitwise-and (arithmetic-shift v (- shift)) #xFF)))
         (for ([b (if big-endian? (reverse bs) bs)])
           (write-byte b out))
         (set! val-idx (+ val-idx 1)))]
      [(#\z)
       (when (< val-idx (length vals))
         (define s (~a (list-ref vals val-idx)))
         (display s out)
         (write-byte 0 out)
         (set! val-idx (+ val-idx 1)))]))
  (bytes->string/latin-1 (get-output-bytes out)))

(define (nl-unpack args)
  (define fmt (car args))
  (define data (cadr args))
  (define bs (if (bytes? data) data (string->bytes/latin-1 (if (string? data) data (~a data)))))
  (define in (open-input-bytes bs))
  (define big-endian? #f)
  (define res '())
  (for ([ch (string->list fmt)])
    (case ch
      [(#\>) (set! big-endian? #t)]
      [(#\<) (set! big-endian? #f)]
      [(#\b)
       (define b (read-byte in))
       (unless (eof-object? b) (set! res (cons b res)))]
      [(#\c)
       (define b (read-byte in))
       (unless (eof-object? b)
         (set! res (cons (if (> b 127) (- b 256) b) res)))]
      [(#\s #\u)
       (define b1 (read-byte in))
       (define b2 (read-byte in))
       (unless (or (eof-object? b1) (eof-object? b2))
         (define v (if big-endian? (+ (arithmetic-shift b1 8) b2) (+ (arithmetic-shift b2 8) b1)))
         (set! res (cons (if (and (char=? ch #\s) (> v 32767)) (- v 65536) v) res)))]
      [(#\d #\l)
       (define bytes (for/list ([i 4]) (read-byte in)))
       (unless (ormap eof-object? bytes)
         (define ordered (if big-endian? bytes (reverse bytes)))
         (define v (for/fold ([acc 0]) ([b ordered]) (+ (arithmetic-shift acc 8) b)))
         (set! res (cons (if (> v #x7FFFFFFF) (- v #x100000000) v) res)))]
      [(#\z)
       (define str-bytes
         (let loop ([acc '()])
           (define b (read-byte in))
           (if (or (eof-object? b) (= b 0))
               (reverse acc)
               (loop (cons b acc)))))
       (set! res (cons (bytes->string/utf-8 (list->bytes str-bytes)) res))]))
  (reverse res))

(define (nl-struct args)
  (define sym (car args))
  (define fmt (cadr args))
  (list 'struct sym fmt))

(define (nl-get-char args)
  (define offset (car args))
  (define str (cadr args))
  (define bs (if (bytes? str) str (string->bytes/latin-1 (if (string? str) str (~a str)))))
  (if (< offset (bytes-length bs))
      (bytes-ref bs offset)
      nl-nil))

(define (nl-get-int args)
  (define offset (car args))
  (define str (cadr args))
  (define bs (if (bytes? str) str (string->bytes/latin-1 (if (string? str) str (~a str)))))
  (if (<= (+ offset 4) (bytes-length bs))
      (+ (bytes-ref bs offset)
         (arithmetic-shift (bytes-ref bs (+ offset 1)) 8)
         (arithmetic-shift (bytes-ref bs (+ offset 2)) 16)
         (arithmetic-shift (bytes-ref bs (+ offset 3)) 24))
      nl-nil))

(define (nl-get-long args)
  (nl-get-int args))

(define (nl-get-float args)
  (exact->inexact (or (nl-get-int args) 0)))

(define (nl-get-string args)
  (define offset (car args))
  (define len (cadr args))
  (define str (caddr args))
  (define bs (if (bytes? str) str (string->bytes/latin-1 (if (string? str) str (~a str)))))
  (if (<= (+ offset len) (bytes-length bs))
      (bytes->string/latin-1 (subbytes bs offset (+ offset len)))
      nl-nil))

;; -------------------------------------------------------------------
;; List Processing & Pattern Matching
;; -------------------------------------------------------------------

(define (nl-exists args)
  (define func (car args))
  (define lst (cadr args))
  (let loop ([rem lst])
    (if (null? rem)
        nl-nil
        (let ([res (apply-evaluated func (list (car rem)))])
          (if (nl-truthy? res)
              res
              (loop (cdr rem)))))))

(define (nl-for-all args)
  (define func (car args))
  (define lst (cadr args))
  (let loop ([rem lst])
    (if (null? rem)
        nl-true
        (let ([res (apply-evaluated func (list (car rem)))])
          (if (nl-truthy? res)
              (loop (cdr rem))
              nl-nil)))))

(define (nl-index args)
  (define target (car args))
  (define lst (cadr args))
  (define is-func? (or (nl-lambda? target) (nl-primitive? target)))
  (for/list ([item lst] [i (in-naturals)]
             #:when (if is-func?
                        (nl-truthy? (apply-evaluated target (list item)))
                        (equal? item target)))
    i))

(define (nl-select args)
  (define lst (car args))
  (define indices (cadr args))
  (define n (length lst))
  (for/list ([idx indices])
    (define norm-idx (if (< idx 0) (+ n idx) idx))
    (if (and (>= norm-idx 0) (< norm-idx n))
        (list-ref lst norm-idx)
        nl-nil)))

(define (nl-rotate args)
  (define target (car args))
  (define offset (if (pair? (cdr args)) (cadr args) 1))
  (cond
    [(list? target)
     (define n (length target))
     (if (zero? n)
         '()
         (let ([shift (modulo (- offset) n)])
           (append (drop target shift) (take target shift))))]
    [(string? target)
     (define n (string-length target))
     (if (zero? n)
         ""
         (let ([shift (modulo (- offset) n)])
           (string-append (substring target shift) (substring target 0 shift))))]
    [else target]))

(define (nl-extend args)
  (define place (car args))
  (define elems (cdr args))
  (cond
    [(list? place)
     (append place elems)]
    [(string? place)
     (string-join (cons place (map ~a elems)) "")]
    [else place]))

(define (nl-set-ref args)
  (define idx (car args))
  (define lst (cadr args))
  (define val (caddr args))
  (if (list? lst)
      (list-set lst idx val)
      lst))

(define (nl-set-ref-all args)
  (define target (car args))
  (define lst (cadr args))
  (define val (caddr args))
  (define (replace-all tree)
    (cond
      [(equal? tree target) val]
      [(pair? tree) (cons (replace-all (car tree)) (replace-all (cdr tree)))]
      [else tree]))
  (replace-all lst))

(define (nl-pop-assoc args)
  (define key (car args))
  (define place (cadr args))
  (cond
    [(nl-symbol? place)
     (define lst (nl-symbol-value place))
     (if (list? lst)
         (let-values ([(found rest-lst)
                       (let loop ([rem lst] [acc '()])
                         (cond
                           [(null? rem) (values nl-nil lst)]
                           [(and (pair? (car rem)) (equal? (caar rem) key))
                            (values (car rem) (append (reverse acc) (cdr rem)))]
                           [else (loop (cdr rem) (cons (car rem) acc))]))])
           (set-symbol-val! place rest-lst)
           found)
         nl-nil)]
    [else nl-nil]))

(define (nl-union args)
  (remove-duplicates (apply append args)))

;; Wildcard Pattern Matching (? and *)
(define (nl-match args)
  (define pat (car args))
  (define lst (cadr args))
  (unless (and (list? pat) (list? lst))
    (error 'match "expected lists for pattern and target"))
  (define (is-wildcard-q? elem)
    (and (nl-symbol? elem) (string=? (nl-symbol-name elem) "?")))
  (define (is-wildcard-star? elem)
    (and (nl-symbol? elem) (string=? (nl-symbol-name elem) "*")))

  (let loop ([p pat] [l lst] [matches '()])
    (cond
      [(and (null? p) (null? l)) (reverse matches)]
      [(null? p) nl-nil]
      [(is-wildcard-q? (car p))
       (if (null? l)
           nl-nil
           (loop (cdr p) (cdr l) (cons (car l) matches)))]
      [(is-wildcard-star? (car p))
       (let star-loop ([len 0])
         (if (> len (length l))
             nl-nil
             (let* ([prefix (take l len)]
                    [suffix (drop l len)]
                    [res (loop (cdr p) suffix (cons prefix matches))])
               (if (not (nl-nil? res))
                   res
                   (star-loop (+ len 1))))))]
      [(null? l) nl-nil]
      [(equal? (car p) (car l))
       (loop (cdr p) (cdr l) matches)]
      [else nl-nil])))

(define (nl-unify args)
  (define p1 (car args))
  (define p2 (cadr args))
  (if (equal? p1 p2)
      p1
      (nl-match (list p1 p2))))

;; -------------------------------------------------------------------
;; Date & Time Functions
;; -------------------------------------------------------------------

(define (nl-date-list args)
  (define secs (if (pair? args) (car args) (current-seconds)))
  (define idx (if (and (pair? args) (pair? (cdr args))) (cadr args) #f))
  (define d (seconds->date secs #t))
  (define lst
    (list (date-year d)
          (date-month d)
          (date-day d)
          (date-hour d)
          (date-minute d)
          (date-second d)
          (if (date-dst? d) 1 0)
          (date-year-day d)
          (date-week-day d)
          (date-time-zone-offset d)))
  (if idx
      (if (and (>= idx 0) (< idx 10))
          (list-ref lst idx)
          nl-nil)
      lst))

(define (nl-date-value args)
  (cond
    [(null? args) (current-seconds)]
    [(= (length args) 1)
     (define v (car args))
     (if (list? v)
         (find-seconds (if (>= (length v) 6) (list-ref v 5) 0)
                       (if (>= (length v) 5) (list-ref v 4) 0)
                       (if (>= (length v) 4) (list-ref v 3) 0)
                       (if (>= (length v) 3) (list-ref v 2) 1)
                       (if (>= (length v) 2) (list-ref v 1) 1)
                       (if (>= (length v) 1) (list-ref v 0) 1970))
         (current-seconds))]
    [(>= (length args) 3)
     (find-seconds (if (>= (length args) 6) (list-ref args 5) 0)
                   (if (>= (length args) 5) (list-ref args 4) 0)
                   (if (>= (length args) 4) (list-ref args 3) 0)
                   (list-ref args 2)
                   (list-ref args 1)
                   (list-ref args 0))]
    [else (current-seconds)]))

(define (nl-date-parse args)
  (define str (car args))
  (define fmt (if (pair? (cdr args)) (cadr args) "%Y-%m-%d"))
  (current-seconds))

;; -------------------------------------------------------------------
;; System, Process & Events
;; -------------------------------------------------------------------

(define (nl-shell-exec args)
  (define cmd (car args))
  (system cmd))

(define (nl-regex-dollar args)
  (define idx (car args))
  (define m (*last-regex-matches*))
  (if (and (list? m) (< idx (length m)))
      (list-ref m idx)
      nl-nil))

(define (nl-delete args)
  (define target (car args))
  (define all? (and (pair? (cdr args)) (nl-truthy? (cadr args))))
  (cond
    [(nl-symbol? target)
     (define ctx (hash-ref global-contexts (nl-symbol-context-name target) #f))
     (when ctx
       (hash-remove! (nl-context-symbols ctx) (nl-symbol-name target)))
     nl-true]
    [(nl-context? target)
     (hash-remove! global-contexts (nl-context-name target))
     nl-true]
    [else nl-nil]))

(define (nl-reset)
  (*last-error-msg* "no error")
  nl-true)

(define *max-cells* (make-parameter 268435456))
(define *stack-size* (make-parameter 2048))

(define (nl-sys-info [args '()])
  (define os-code
    (case (system-type 'os)
      [(windows) 6]
      [(macosx) 3]
      [else 1]))
  (define os-val (+ os-code 256 128)) ;; 64-bit + UTF-8
  (define pid (getpid))
  (define info-list
    (list 429
          (*max-cells*)
          402
          1
          0
          (*stack-size*)
          0
          pid
          10706
          os-val))
  (if (null? args)
      info-list
      (let* ([idx (car args)]
             [len (length info-list)]
             [actual-idx (if (< idx 0) (+ len idx) idx)])
        (if (and (>= actual-idx 0) (< actual-idx len))
            (list-ref info-list actual-idx)
            nl-nil))))

(define (nl-sys-error args)
  (if (pair? args)
      (format "Error ~a" (car args))
      (*last-error-msg*)))

(define (nl-last-error)
  (*last-error-msg*))

(define (nl-uuid)
  (define (rand-hex len)
    (apply string-append
           (for/list ([i len])
             (~r (random 16) #:base 16 #:min-width 1))))
  (format "~a-~a-4~a-~a~a-~a"
          (rand-hex 8)
          (rand-hex 4)
          (rand-hex 3)
          (list-ref '("8" "9" "a" "b") (random 4))
          (rand-hex 3)
          (rand-hex 12)))

(define (nl-timer args)
  nl-nil)

(define (nl-pretty-print args)
  (for ([item args])
    (displayln (nl->string item #t)))
  nl-nil)

(define (nl-term)
  '(80 24))

(define (nl-set-locale args)
  (if (pair? args)
      (begin
        (*current-locale* (car args))
        (*current-locale*))
      (*current-locale*)))

(define (nl-source args)
  (define fn (car args))
  (if (nl-lambda? fn)
      (list 'lambda (nl-lambda-params fn) (cons 'begin (nl-lambda-body fn)))
      nl-nil))

(define (nl-error-event args)
  (if (pair? args) (*error-event-handler* (car args)) (*error-event-handler*)))

(define (nl-command-event args)
  (if (pair? args) (*command-event-handler* (car args)) (*command-event-handler*)))

(define (nl-prompt-event args)
  (if (pair? args) (*prompt-event-handler* (car args)) (*prompt-event-handler*)))

(define (nl-reader-event args)
  (if (pair? args) (*reader-event-handler* (car args)) (*reader-event-handler*)))

;; Multiprocessing / Cilk Stubs
(define (nl-fork args) 0)
(define (nl-process args) 1)
(define (nl-wait-pid args) 0)
(define (nl-abort args) nl-nil)
(define (nl-destroy args) nl-nil)
(define (nl-spawn args) 1)
(define (nl-sync args) nl-nil)
(define (nl-send args) nl-true)
(define (nl-receive args) nl-nil)
(define (nl-share args) (car args))
(define (nl-semaphore args) nl-true)

;; Browser & FFI Stubs
(define (nl-display-html args) nl-nil)
(define (nl-eval-string-js args) nl-nil)
(define (nl-import args) nl-nil)
(define (nl-dump args) nl-nil)
(define (nl-cpymem args) nl-nil)
(define (nl-history) '())
(define (nl-signal args) nl-nil)
(define (nl-debug args) nl-nil)
(define (nl-trace args) nl-nil)
(define (nl-trace-highlight args) nl-nil)
(define (nl-throw-error args)
  (error 'user-error "~a" (if (pair? args) (car args) "error")))
(define (nl-copy args)
  (car args))
(define (nl-amb args)
  (if (pair? args) (car args) nl-nil))
(define (nl-bayes-train args) nl-true)
(define (nl-bayes-query args) nl-nil)
(define (nl-kmeans-train args) nl-true)
(define (nl-kmeans-query args) nl-nil)
(define (nl-fft args) (car args))
(define (nl-ifft args) (car args))

(define (nl-find args)
  (define target (car args))
  (define source (cadr args))
  (cond
    [(list? source)
     (define compare-fn (if (pair? (cddr args)) (caddr args) #f))
     (let loop ([rem source] [idx 0])
       (cond
         [(null? rem) nl-nil]
         [(if compare-fn
              (nl-truthy? (apply-evaluated compare-fn (list target (car rem))))
              (equal? target (car rem)))
          idx]
         [else (loop (cdr rem) (+ idx 1))]))]
    [(string? source)
     (define offset (if (pair? (cddr args)) (caddr args) 0))
     (define sub (if (string? target) target (~a target)))
     (define rx (regexp (regexp-quote sub)))
     (define m (regexp-match-positions rx source offset))
     (if m
         (caar m)
         nl-nil)]
    [else nl-nil]))

(define (nl-find-all args)
  (define pat (car args))
  (define str (cadr args))
  (define rx (if (regexp? pat) pat (pregexp (if (string? pat) pat (~a pat)))))
  (define matches (regexp-match* rx (if (string? str) str (~a str))))
  (if (pair? (cddr args))
      (let ([trans (caddr args)])
        (if (or (nl-lambda? trans) (nl-primitive? trans))
            (map (lambda (m) (apply-evaluated trans (list m))) matches)
            matches))
      matches))

(define (nl-ref-all args)
  (define target (car args))
  (define tree (cadr args))
  (define compare-fn (if (pair? (cddr args)) (caddr args) #f))
  (define (matches? val)
    (if compare-fn
        (nl-truthy? (apply-evaluated compare-fn (list target val)))
        (equal? target val)))
  (define (search node path)
    (cond
      [(matches? node) (list (reverse path))]
      [(list? node)
       (for/fold ([acc '()]) ([elem node] [idx (in-naturals)])
         (append acc (search elem (cons idx path))))]
      [else '()]))
  (search tree '()))

(define (nl-callback args)
  (if (pair? args) (car args) 0))

(define (nl-pipe)
  (define-values (in-p out-p) (make-pipe))
  (list (allocate-file-handle in-p #f "read" "pipe:in")
        (allocate-file-handle #f out-p "write" "pipe:out")))
