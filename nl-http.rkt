#lang racket/base

(require racket/string
         racket/list
         racket/port
         racket/file
         racket/format
         net/url
         net/http-client
         net/base64
         xml
         json
         "nl-types.rkt")

(provide (all-defined-out))

;; Global state for errors and hooks
(define *last-json-error* (make-parameter nl-nil))
(define *last-xml-error* (make-parameter nl-nil))
(define *xml-type-tags* (make-parameter '()))
(define *xfer-event-handler* (make-parameter #f))

;; Helper to convert URL path to clean string
(define (url-path->string p)
  (string-join (map (lambda (seg)
                      (if (path/param? seg)
                          (path/param-path seg)
                          (~a seg)))
                    p)
               "/"))

;; Parse arguments according to newLISP manual:
;; (get-url url [opt] [timeout [header]]) or (get-url url timeout [header])
;; (post-url url data [opt] [timeout [header]]) or (post-url url data timeout [header])
(define (parse-http-args args [has-data? #f])
  (define url-str (car args))
  (define rest-args (cdr args))
  (define data (if has-data? (and (pair? rest-args) (car rest-args)) #f))
  (define tail (if has-data? (if (pair? rest-args) (cdr rest-args) '()) rest-args))
  (define opt "")
  (define timeout #f)
  (define custom-header #f)
  (cond
    [(null? tail) (void)]
    [(number? (car tail))
     (set! timeout (car tail))
     (when (pair? (cdr tail)) (set! custom-header (cadr tail)))]
    [(string? (car tail))
     (set! opt (car tail))
     (when (pair? (cdr tail))
       (if (number? (cadr tail))
           (begin
             (set! timeout (cadr tail))
             (when (pair? (cddr tail)) (set! custom-header (caddr tail))))
           (set! custom-header (cadr tail))))])
  (values url-str data opt timeout custom-header))

;; Core HTTP / file:// Request Dispatcher
(define (perform-http-request method url-str data opt timeout custom-header [redirect-count 0])
  ;; 1. Check file:// URL
  (if (string-prefix? url-str "file://")
      (let* ([raw-path (substring url-str 7)]
             ;; Handle file:///c:/... or file:///home/... or file://localhost/...
             [clean-path
              (cond
                [(and (> (string-length raw-path) 2)
                      (char=? (string-ref raw-path 0) #\/)
                      (char-alphabetic? (string-ref raw-path 1))
                      (char=? (string-ref raw-path 2) #\:))
                 (substring raw-path 1)]
                [(string-prefix? raw-path "localhost/")
                 (substring raw-path 9)]
                [else raw-path])])
        (if (file-exists? clean-path)
            (let ([content (file->string clean-path)])
              (cond
                [(string-contains? opt "header")
                 (format "Content-Type: text/plain\r\nContent-Length: ~a\r\n" (string-length content))]
                [(string-contains? opt "list")
                 (list (format "Content-Type: text/plain\r\nContent-Length: ~a\r\n" (string-length content))
                       content
                       200)]
                [else content]))
            (string-append "ERR: file not found: " clean-path)))

      ;; 2. HTTP / HTTPS Request
      (let ()
        (define (do-request)
          (define u (string->url url-str))
          (define is-ssl? (equal? (url-scheme u) "https"))
          (define default-port (if is-ssl? 443 80))
          (define port-num (or (url-port u) default-port))
          (define host-str (url-host u))
          (unless host-str
            (error 'http "invalid URL: ~a" url-str))

          ;; Path and query construction
          (define stripped-url (struct-copy url u [scheme #f] [host #f] [port #f]))
          (define rel-path (url->string stripped-url))
          (define full-path
            (if (or (string=? rel-path "") (not (string-prefix? rel-path "/")))
                (string-append "/" rel-path)
                rel-path))

          ;; Headers construction
          (define req-headers
            (if custom-header
                (filter (lambda (s) (> (string-length s) 0))
                        (string-split custom-header "\r\n"))
                (list "User-Agent: newLISP v10706"
                      "Connection: close")))

          (when (string-contains? opt "debug")
            (eprintf "--> ~a ~a HTTP/1.1\nHost: ~a\n~a\n\n"
                     method full-path host-str
                     (string-join req-headers "\n")))

          (define post-bytes
            (if data
                (string->bytes/utf-8 (if (string? data) data (~a data)))
                #f))

          (define-values (status-line resp-headers in-port)
            (http-sendrecv host-str
                           full-path
                           #:ssl? is-ssl?
                           #:port port-num
                           #:method method
                           #:headers (map string->bytes/utf-8 req-headers)
                           #:data post-bytes))

          (define status-str (bytes->string/utf-8 status-line))
          (define header-str (string-join (map bytes->string/utf-8 resp-headers) "\r\n"))
          (define body-str (port->string in-port))
          (close-input-port in-port)

          ;; Extract status code
          (define status-parts (string-split status-str " "))
          (define status-code (if (>= (length status-parts) 2)
                                  (or (string->number (cadr status-parts)) 200)
                                  200))

          ;; Redirection handling (301, 302, 303, 307, 308)
          (define is-redirect? (and (member status-code '(301 302 303 307 308))
                                   (not (string-contains? opt "raw"))
                                   (< redirect-count 10)))
          (define location-hdr
            (and is-redirect?
                 (for/first ([h resp-headers]
                             #:when (string-prefix? (string-downcase (bytes->string/utf-8 h)) "location:"))
                   (string-trim (substring (bytes->string/utf-8 h) 9)))))

          (cond
            [location-hdr
             (define next-url
               (if (or (string-prefix? location-hdr "http://") (string-prefix? location-hdr "https://"))
                   location-hdr
                   (url->string (combine-url/relative u location-hdr))))
             (perform-http-request method next-url data opt timeout custom-header (+ redirect-count 1))]
            [(string-contains? opt "header") header-str]
            [(string-contains? opt "list")
             (list header-str body-str status-code)]
            [else body-str]))

        ;; Timeout handling
        (with-handlers ([exn:fail? (lambda (ex) (string-append "ERR: " (exn-message ex)))])
          (if timeout
              (let* ([t-channel (make-channel)]
                     [worker (thread (lambda ()
                                       (with-handlers ([exn:fail? (lambda (e) (channel-put t-channel e))])
                                         (channel-put t-channel (do-request)))))]
                     [res (sync/timeout (/ timeout 1000.0) t-channel)])
                (if res
                    (if (exn? res)
                        (string-append "ERR: " (exn-message res))
                        res)
                    (begin
                      (kill-thread worker)
                      "ERR: timeout")))
              (do-request))))))

(define (nl-get-url args)
  (define-values (url-str data opt timeout custom-header) (parse-http-args args #f))
  (perform-http-request #"GET" url-str #f opt timeout custom-header))

(define (nl-post-url args)
  (define-values (url-str data opt timeout custom-header) (parse-http-args args #t))
  (perform-http-request #"POST" url-str data opt timeout custom-header))

(define (nl-put-url args)
  (define-values (url-str data opt timeout custom-header) (parse-http-args args #t))
  (perform-http-request #"PUT" url-str data opt timeout custom-header))

(define (nl-delete-url args)
  (define-values (url-str data opt timeout custom-header) (parse-http-args args #f))
  (perform-http-request #"DELETE" url-str #f opt timeout custom-header))

;; Base64 Encode & Decode
(define (nl-base64-enc args)
  (define str (car args))
  (define no-lf? (and (pair? (cdr args)) (nl-truthy? (cadr args))))
  (define enc (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 str))))
  (if no-lf?
      (string-trim (string-replace enc "\r\n" "") "\n")
      enc))

(define (nl-base64-dec args)
  (define str (car args))
  (bytes->string/utf-8 (base64-decode (string->bytes/utf-8 str))))

;; JSON Error
(define (nl-json-error)
  (*last-json-error*))

;; XML Parsing into newLISP S-XML format
(define (nl-xml-parse args)
  (define xml-str (car args))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (*last-xml-error* (string-append "ERR: " (exn-message e)))
                     nl-nil)])
    (define x (read-xml/element (open-input-string xml-str)))
    (define (convert-xml elem)
      (cond
        [(element? elem)
         (define tag (symbol->string (element-name elem)))
         (define attrs
           (for/list ([a (element-attributes elem)])
             (list (symbol->string (attribute-name a))
                   (attribute-value a))))
         (define content
           (filter (lambda (v) (not (and (string? v) (string=? (string-trim v) ""))))
                   (map convert-xml (element-content elem))))
         (list tag (if (null? attrs) '() attrs) content)]
        [(string? elem) elem]
        [(pcdata? elem) (pcdata-string elem)]
        [else nl-nil]))
    (convert-xml x)))

(define (nl-xml-error)
  (*last-xml-error*))

(define (nl-xml-type-tags args)
  (if (null? args)
      (*xml-type-tags*)
      (begin
        (*xml-type-tags* (car args))
        (*xml-type-tags*))))

(define (nl-xfer-event args)
  (if (null? args)
      (*xfer-event-handler*)
      (begin
        (*xfer-event-handler* (car args))
        (*xfer-event-handler*))))
