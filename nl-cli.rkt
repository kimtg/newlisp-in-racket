#lang racket/base

(require racket/file
         racket/string
         racket/list
         racket/tcp
         racket/port
         racket/date
         racket/system
         racket/format
         racket/path
         "nl-types.rkt"
         "nl-reader.rkt"
         "nl-eval.rkt"
         "nl-builtins.rkt"
         "nl-transpile.rkt"
         "nl-repl.rkt"
         "nl-ext.rkt"
         "nl-net.rkt"
         "nl-http.rkt")

(provide (all-defined-out))

(define HELP-TEXT
#" -h this help (no init.lsp)
 -n no init.lsp (must be first)
 -x <source> <target> link (no init.lsp)
 -v version
 -s <stacksize>
 -m <max-mem-MB> cell memory
 -e <quoted lisp expression>
 -l <path-file> log connections
 -L <path-file> log all
 -w <working dir>
 -c no prompts, HTTP
 -C force prompts
 -t <usec-server-timeout>
 -p <port-no>
 -d <port-no> demon mode
 -http only
 -http-safe safe mode
 -6 IPv6 mode
")

(define MAGIC-TRAILER #"NEWLISP_EMBEDDED_MAGIC\n")
(define TRAILER-LEN (bytes-length MAGIC-TRAILER))
(define HEADER-LEN 10)
(define FOOTER-LEN (+ HEADER-LEN TRAILER-LEN))

;; Evaluate expression string using compiler if possible, falling back to interpreter
(define (eval-smart code-str)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define exprs
                       (nl-read-all code-str (lambda (s) (find-or-create-symbol s (current-context)))))
                     (eval-body exprs))])
    (eval-compiled-string code-str)))

;; Check if current running executable has linked/embedded code
(define (get-embedded-source [exe-path #f])
  (define path (or exe-path (find-system-path 'run-file)))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (if (and path (file-exists? path))
        (let ([sz (file-size path)])
          (if (< sz FOOTER-LEN)
              #f
              (call-with-input-file path
                (lambda (in)
                  (file-position in (- sz FOOTER-LEN))
                  (define len-bytes (read-bytes HEADER-LEN in))
                  (define magic (read-bytes TRAILER-LEN in))
                  (if (and (equal? magic MAGIC-TRAILER)
                           (regexp-match? #px"^[0-9]+$" len-bytes))
                      (let* ([payload-len (string->number (bytes->string/utf-8 len-bytes))]
                             [start-pos (- sz FOOTER-LEN payload-len)])
                        (if (and (number? payload-len) (>= start-pos 0))
                            (let ()
                              (file-position in start-pos)
                              (define raw-payload (read-bytes payload-len in))
                              (define payload-str (bytes->string/utf-8 raw-payload))
                              (define m (regexp-match #px";; --- NEWLISP-EMBEDDED-START ---\n([\\s\\S]*)\n;; --- NEWLISP-EMBEDDED-END ---" payload-str))
                              (if m (cadr m) payload-str))
                            #f))
                      #f))
                #:mode 'binary)))
        #f)))

;; Link source into target standalone executable
(define (link-executable source-file target-file)
  (unless (file-exists? source-file)
    (eprintf "ERR: cannot open source file: ~a\n" source-file)
    (exit 1))
  ;; Locate base executable
  (define base-exe
    (let ([cand1 (find-system-path 'run-file)])
      (cond
        [(and (file-exists? cand1)
              (let ([name (path->string (file-name-from-path cand1))])
                (or (string-ci=? name "newlisp.exe") (string-ci=? name "newlisp"))))
         cand1]
        [(file-exists? "newlisp.exe") "newlisp.exe"]
        [(file-exists? "newlisp") "newlisp"]
        [(let ([p (build-path (or (path-only cand1) ".") "newlisp.exe")])
           (and (file-exists? p) p))]
        [(let ([p (build-path (or (path-only cand1) ".") "newlisp")])
           (and (file-exists? p) p))]
        [(find-executable-path "newlisp.exe")]
        [(find-executable-path "newlisp")]
        [else #f])))
  (unless base-exe
    (eprintf "ERR: cannot find newlisp executable to link\n")
    (exit 1))
  (define src-content (file->string source-file))
  (define payload-str
    (string-append "\n;; --- NEWLISP-EMBEDDED-START ---\n"
                   src-content
                   "\n;; --- NEWLISP-EMBEDDED-END ---\n"))
  (define payload-bytes (string->bytes/utf-8 payload-str))
  (define len-str (~r (bytes-length payload-bytes) #:min-width HEADER-LEN #:pad-string "0"))
  (define len-bytes (string->bytes/utf-8 len-str))

  (when (file-exists? target-file) (delete-file target-file))
  (copy-file base-exe target-file #t)

  (call-with-output-file target-file
    (lambda (out)
      (write-bytes payload-bytes out)
      (write-bytes len-bytes out)
      (write-bytes MAGIC-TRAILER out)
      (flush-output out))
    #:mode 'binary
    #:exists 'append)

  (unless (eq? (system-type 'os) 'windows)
    (with-handlers ([exn:fail? void])
      (file-or-directory-permissions target-file #o755)))
  (exit 0))

;; Environment setup for NEWLISPDIR
(define (setup-environment)
  (unless (getenv "NEWLISPDIR")
    (if (eq? (system-type 'os) 'windows)
        (let ([pf (or (getenv "ProgramFiles(x86)") (getenv "ProgramFiles") "C:\\Program Files")])
          (putenv "NEWLISPDIR" (string-append (string-replace pf "\\" "/") "/newlisp")))
        (putenv "NEWLISPDIR" "/usr/local/share/newlisp"))))

;; Load initialization file if present and not suppressed
(define (load-init-file)
  (define home-dir
    (if (eq? (system-type 'os) 'windows)
        (or (getenv "USERPROFILE") (getenv "DOCUMENT_ROOT") (getenv "HOME"))
        (getenv "HOME")))
  (define dot-init (and home-dir (build-path home-dir ".init.lsp")))
  (define dir-init (and (getenv "NEWLISPDIR") (build-path (getenv "NEWLISPDIR") "init.lsp")))
  (cond
    [(and dot-init (file-exists? dot-init))
     (with-handlers ([exn:fail? void])
       (eval-smart (file->string dot-init)))]
    [(and dir-init (file-exists? dir-init))
     (with-handlers ([exn:fail? void])
       (eval-smart (file->string dir-init)))]))

;; MIME type lookup for HTTP server
(define (get-mime-type path-str)
  (define raw-ext (or (path-get-extension (string->path path-str)) #""))
  (define ext (string-downcase (bytes->string/utf-8 raw-ext)))
  (cond
    [(string=? ext ".avi") "video/x-msvideo"]
    [(string=? ext ".css") "text/css"]
    [(string=? ext ".gif") "image/gif"]
    [(or (string=? ext ".htm") (string=? ext ".html")) "text/html"]
    [(or (string=? ext ".jpg") (string=? ext ".jpeg")) "image/jpeg"]
    [(string=? ext ".js") "application/javascript"]
    [(string=? ext ".mov") "video/quicktime"]
    [(string=? ext ".mp3") "audio/mpeg"]
    [(string=? ext ".mpg") "video/mpeg"]
    [(string=? ext ".pdf") "application/pdf"]
    [(string=? ext ".png") "image/png"]
    [(string=? ext ".wav") "audio/x-wav"]
    [(string=? ext ".zip") "application/zip"]
    [else "text/plain"]))

;; TCP/HTTP Server implementation
(define (run-server #:port port
                    #:daemon? [daemon? #f]
                    #:http-only? [http-only? #f]
                    #:http-safe? [http-safe? #f]
                    #:ipv6? [ipv6? #f]
                    #:timeout-us [timeout-us #f]
                    #:log-file [log-file #f]
                    #:log-all? [log-all? #f]
                    #:no-prompt? [no-prompt? #f])
  (define port-num (if (string? port) (or (string->number port) 8080) port))
  (define host (if ipv6? "::" #f))
  (define listener
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (eprintf "ERR: cannot open server on port ~a: ~a\n" port-num (exn-message e))
                       (exit 1))])
      (tcp-listen port-num 128 #t host)))

  (when log-file
    (log-write log-file
               (format "[~a] Server started on port ~a (daemon: ~a, http-only: ~a)\n"
                       (date->string (current-date) #t) port-num daemon? http-only?)))

  (let accept-loop ()
    (define-values (cin cout) (tcp-accept listener))
    (define-values (lh lp ph pp) (tcp-addresses cin #t))
    (when log-file
      (log-write log-file
                 (format "[~a] Connection accepted from ~a:~a\n"
                         (date->string (current-date) #t) ph pp)))

    (define timeout-secs (and timeout-us (> timeout-us 0) (/ timeout-us 1000000.0)))

    ;; Read line with optional timeout
    (define (timed-read-line)
      (if timeout-secs
          (let ([ch (make-channel)])
            (define t (thread (lambda () (channel-put ch (read-line cin 'any)))))
            (define res (sync/timeout timeout-secs ch))
            (if res
                res
                (begin (kill-thread t) eof)))
          (read-line cin 'any)))

    (define first-line (timed-read-line))
    (cond
      [(eof-object? first-line)
       (close-input-port cin)
       (close-output-port cout)
       (if daemon?
           (begin (current-context main-context) (accept-loop))
           (exit 0))]

      ;; HTTP Request Dispatch
      [(and (string? first-line)
            (or (string-prefix? first-line "GET ")
                (string-prefix? first-line "POST ")
                (string-prefix? first-line "PUT ")
                (string-prefix? first-line "DELETE ")
                (string-prefix? first-line "HEAD ")))
       (handle-http-request first-line cin cout lh lp ph pp
                            #:http-safe? http-safe?
                            #:log-file log-file
                            #:log-all? log-all?)
       (close-input-port cin)
       (close-output-port cout)
       (if daemon?
           (begin (current-context main-context) (accept-loop))
           (exit 0))]

      ;; Non-HTTP in HTTP-only mode: reject and close
      [http-only?
       (close-input-port cin)
       (close-output-port cout)
       (if daemon?
           (begin (current-context main-context) (accept-loop))
           (exit 0))]

      ;; Lisp Command / Socket Mode
      [else
       (handle-socket-lisp-session first-line cin cout timed-read-line
                                   #:no-prompt? no-prompt?
                                   #:log-file log-file
                                   #:log-all? log-all?)
       (close-input-port cin)
       (close-output-port cout)
       (if daemon?
           (begin (current-context main-context) (accept-loop))
           (exit 0))])))

;; Handle HTTP request over socket
(define (handle-http-request first-line cin cout lh lp ph pp
                             #:http-safe? http-safe?
                             #:log-file log-file
                             #:log-all? log-all?)
  (define parts (string-split (string-trim first-line) " "))
  (define method (if (pair? parts) (car parts) "GET"))
  (define raw-uri (if (>= (length parts) 2) (cadr parts) "/"))
  (define uri-parts (string-split raw-uri "?"))
  (define req-path (car uri-parts))
  (define query-str (if (pair? (cdr uri-parts)) (string-join (cdr uri-parts) "?") ""))

  ;; Read headers until empty line
  (define headers
    (let loop ([hdrs '()])
      (define line (read-line cin 'any))
      (if (or (eof-object? line) (string=? (string-trim line) ""))
          (reverse hdrs)
          (loop (cons line hdrs)))))

  ;; Header helper
  (define (get-hdr-val prefix)
    (for/first ([h headers]
                #:when (string-prefix? (string-downcase h) (string-downcase prefix)))
      (string-trim (substring h (string-length prefix)))))

  ;; Set CGI environment variables as specified in newLISP manual
  (putenv "DOCUMENT_ROOT" (path->string (current-directory)))
  (putenv "HTTP_HOST" (or (get-hdr-val "Host:") ""))
  (putenv "REMOTE_ADDR" ph)
  (putenv "REQUEST_METHOD" method)
  (putenv "REQUEST_URI" raw-uri)
  (putenv "SERVER_SOFTWARE" "newLISP v.10.7.6")
  (putenv "QUERY_STRING" query-str)
  (when (get-hdr-val "Content-Type:") (putenv "CONTENT_TYPE" (get-hdr-val "Content-Type:")))
  (when (get-hdr-val "Content-Length:") (putenv "CONTENT_LENGTH" (get-hdr-val "Content-Length:")))
  (when (get-hdr-val "User-Agent:") (putenv "HTTP_USER_AGENT" (get-hdr-val "User-Agent:")))
  (when (get-hdr-val "Cookie:") (putenv "HTTP_COOKIE" (get-hdr-val "Cookie:")))

  ;; Pre-process request via command-event if registered
  (define final-path
    (if (*command-event-handler*)
        (with-handlers ([exn:fail? (lambda (e) req-path)])
          (define translated (nl-eval (list (*command-event-handler*) req-path)))
          (if (string? translated) translated req-path))
        req-path))

  ;; Check safe mode path traversal
  (define is-safe?
    (not (or (string-contains? final-path "..")
             (string-contains? final-path "//"))))

  (cond
    [(and http-safe? (not is-safe?))
     (define body "<html><body><h1>403 Forbidden</h1></body></html>")
     (display (format "HTTP/1.1 403 Forbidden\r\nContent-Type: text/html\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a"
                      (string-length body) body) cout)
     (flush-output cout)
     (when log-all?
       (log-write log-file (format "[~a] HTTP: ~a ~a -> 403 Forbidden\n" (date->string (current-date) #t) method raw-uri)))]

    [else
     (define clean-rel
       (let ([p (if (string-prefix? final-path "/") (substring final-path 1) final-path)])
         (if (or (string=? p "") (string-suffix? p "/"))
             (string-append p "index.html")
             p)))
     (define file-to-serve (build-path (current-directory) clean-rel))
     (if (and (file-exists? file-to-serve) (not (directory-exists? file-to-serve)))
         (let* ([content-bytes (file->bytes file-to-serve)]
                [mime (get-mime-type (path->string file-to-serve))]
                [len (bytes-length content-bytes)])
           (display (format "HTTP/1.1 200 OK\r\nContent-Type: ~a\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n"
                            mime len) cout)
           (write-bytes content-bytes cout)
           (flush-output cout)
           (when log-all?
             (log-write log-file (format "[~a] HTTP: ~a ~a -> 200 OK (~a bytes, ~a)\n"
                                         (date->string (current-date) #t) method raw-uri len mime))))
         (let ([body "<html><body><h1>404 Not Found</h1></body></html>"])
           (display (format "HTTP/1.1 404 Not Found\r\nContent-Type: text/html\r\nContent-Length: ~a\r\nConnection: close\r\n\r\n~a"
                            (string-length body) body) cout)
           (flush-output cout)
           (when log-all?
             (log-write log-file (format "[~a] HTTP: ~a ~a -> 404 Not Found\n" (date->string (current-date) #t) method raw-uri)))))]))

;; Handle socket Lisp session
(define (handle-socket-lisp-session initial-line cin cout timed-read-line
                                    #:no-prompt? no-prompt?
                                    #:log-file log-file
                                    #:log-all? log-all?)
  (unless no-prompt?
    (display (get-banner) cout)
    (display "> " cout)
    (flush-output cout))

  (let loop ([current-line initial-line])
    (cond
      [(eof-object? current-line)
       (void)]
      [else
       (define trimmed (string-trim current-line))
       (cond
         [(string=? trimmed "")
          (unless no-prompt? (display "> " cout) (flush-output cout))
          (loop (timed-read-line))]
         [(string=? trimmed "(exit)")
          (void)]
         [(string=? trimmed "[cmd]")
          (define full-cmd
            (let accum ([lines '()])
              (define l (timed-read-line))
              (cond
                [(or (eof-object? l) (string=? (string-trim l) "[/cmd]"))
                 (string-join (reverse lines) "\n")]
                [else (accum (cons l lines))])))
          (when log-file
            (log-write log-file (format "[~a] SOCKET IN: [cmd]\n~a\n[/cmd]\n" (date->string (current-date) #t) full-cmd)))
          (eval-socket-command full-cmd cout log-file log-all? no-prompt?)
          (loop (timed-read-line))]
         [else
          (when log-file
            (log-write log-file (format "[~a] SOCKET IN: ~a\n" (date->string (current-date) #t) trimmed)))
          (eval-socket-command trimmed cout log-file log-all? no-prompt?)
          (loop (timed-read-line))])])))

(define (eval-socket-command cmd-str cout log-file log-all? no-prompt?)
  (define final-cmd
    (if (*command-event-handler*)
        (with-handlers ([exn:fail? (lambda (e) cmd-str)])
          (define tr (nl-eval (list (*command-event-handler*) cmd-str)))
          (if (string? tr) tr cmd-str))
        cmd-str))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (define err-str (format "ERR: ~a\n" (exn-message e)))
                     (display err-str cout)
                     (unless no-prompt? (display "> " cout))
                     (flush-output cout)
                     (when log-all?
                       (log-write log-file (format "[~a] SOCKET OUT: ~a" (date->string (current-date) #t) err-str))))])
    (define exprs (nl-read-all final-cmd (lambda (s) (find-or-create-symbol s (current-context)))))
    (for ([expr exprs])
      (define res
        (with-handlers ([exn:fail? (lambda (e) (nl-eval expr))])
          (eval-compiled expr)))
      (define res-str (nl->string res #t))
      (displayln res-str cout)
      (when log-all?
        (log-write log-file (format "[~a] SOCKET OUT: ~a\n" (date->string (current-date) #t) res-str))))
    (unless no-prompt?
      (display "> " cout))
    (flush-output cout)))

;; -------------------------------------------------------------------
;; Main CLI Parsing & Entry Point
;; -------------------------------------------------------------------

(define (run-cli [raw-argv (vector->list (current-command-line-arguments))])
  ;; Check for embedded binary code first
  (define embedded-code (get-embedded-source))
  (if embedded-code
      (let ()
        ;; Linked binary execution
        (define prog-name (path->string (find-system-path 'run-file)))
        (define main-args-list (cons prog-name raw-argv))
        (set-symbol-val! sym-dollar-main-args main-args-list)
        (setup-environment)
        (eval-smart embedded-code))

      ;; Normal CLI processing
      (process-normal-cli raw-argv)))

(define (process-normal-cli raw-argv)
  (define prog-name
    (let ([run-f (path->string (find-system-path 'run-file))])
      (if (or (string-contains? (string-downcase run-f) "racket")
              (string-contains? (string-downcase run-f) "gracket"))
          "newlisp"
          run-f)))

  ;; Set $main-args early
  (define full-main-args (cons prog-name raw-argv))
  (set-symbol-val! sym-dollar-main-args full-main-args)
  (set-symbol-val! sym-args raw-argv)

  ;; Check -h anywhere or as first option
  (when (member "-h" raw-argv)
    (write-bytes HELP-TEXT (current-output-port))
    (flush-output)
    (exit 0))

  ;; Check -v
  (when (member "-v" raw-argv)
    (display (get-banner))
    (flush-output)
    (exit 0))

  ;; Check -x link mode
  (when (and (pair? raw-argv) (string=? (car raw-argv) "-x"))
    (if (>= (length raw-argv) 3)
        (link-executable (cadr raw-argv) (caddr raw-argv))
        (begin
          (eprintf "ERR: -x requires <source> and <target>\n")
          (exit 1))))

  ;; Setup NEWLISPDIR
  (setup-environment)

  ;; Check -n (must be first option to suppress init.lsp)
  (define suppress-init?
    (and (pair? raw-argv) (string=? (car raw-argv) "-n")))

  (unless suppress-init?
    (load-init-file))

  ;; Remove -n from argument stream if present
  (define argv-stream (if suppress-init? (cdr raw-argv) raw-argv))

  ;; State variables for options
  (define working-dir #f)
  (define no-prompt? #f)
  (define force-prompt? #f)
  (define server-port #f)
  (define daemon-mode? #f)
  (define http-only? #f)
  (define http-safe? #f)
  (define ipv6? #f)
  (define timeout-us #f)
  (define log-file #f)
  (define log-all? #f)
  (define actions '()) ;; list of (cons 'eval str) or (cons 'load str)

  (let loop ([rem argv-stream])
    (cond
      [(null? rem) (void)]

      ;; -s <stacksize> or -s<stacksize>
      [(string-prefix? (car rem) "-s")
       (define arg (car rem))
       (define val-str
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (define n (and val-str (string->number val-str)))
       (when n (*stack-size* n))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -m <max-mem-MB> or -m<max-mem-MB>
      [(string-prefix? (car rem) "-m")
       (define arg (car rem))
       (define val-str
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (define mb (and val-str (string->number val-str)))
       (when mb (*max-cells* (* mb 65536)))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -w <dir> or -w<dir>
      [(string-prefix? (car rem) "-w")
       (define arg (car rem))
       (define dir
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (when dir
         (current-directory dir)
         (set! working-dir dir))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -c
      [(string=? (car rem) "-c")
       (set! no-prompt? #t)
       (loop (cdr rem))]

      ;; -C
      [(string=? (car rem) "-C")
       (set! force-prompt? #t)
       (loop (cdr rem))]

      ;; -6
      [(string=? (car rem) "-6")
       (set! ipv6? #t)
       (*current-ipv* 6)
       (loop (cdr rem))]

      ;; -http-safe
      [(string=? (car rem) "-http-safe")
       (set! http-safe? #t)
       (loop (cdr rem))]

      ;; -http
      [(string=? (car rem) "-http")
       (set! http-only? #t)
       (loop (cdr rem))]

      ;; -t <usec> or -t<usec>
      [(string-prefix? (car rem) "-t")
       (define arg (car rem))
       (define val-str
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (define t (and val-str (string->number val-str)))
       (when t (set! timeout-us t))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -l <log-file> or -l<log-file>
      [(string-prefix? (car rem) "-l")
       (define arg (car rem))
       (define file
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (when file
         (set! log-file file)
         (set! log-all? #f))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -L <log-file> or -L<log-file>
      [(string-prefix? (car rem) "-L")
       (define arg (car rem))
       (define file
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (when file
         (set! log-file file)
         (set! log-all? #t))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -p <port> or -p<port>
      [(string-prefix? (car rem) "-p")
       (define arg (car rem))
       (define p
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (when p
         (set! server-port p)
         (set! daemon-mode? #f))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -d <port> or -d<port>
      [(string-prefix? (car rem) "-d")
       (define arg (car rem))
       (define p
         (if (> (string-length arg) 2)
             (substring arg 2)
             (and (pair? (cdr rem)) (cadr rem))))
       (when p
         (set! server-port p)
         (set! daemon-mode? #t))
       (loop (if (> (string-length arg) 2) (cdr rem) (cddr rem)))]

      ;; -e <expr>
      [(string=? (car rem) "-e")
       (if (pair? (cdr rem))
           (begin
             (set! actions (append actions (list (cons 'eval (cadr rem)))))
             (loop (cddr rem)))
           (loop (cdr rem)))]

      ;; Non-option argument: treated as file/URL to execute if it exists or if no actions yet
      [else
       (define arg (car rem))
       (cond
         [(or (string-prefix? arg "http://")
              (string-prefix? arg "https://")
              (string-prefix? arg "file://")
              (file-exists? arg)
              (null? actions))
          (set! actions (append actions (list (cons 'load arg))))
          (loop (cdr rem))]
         [else
          ;; Non-existing argument after actions already queued: treated as command-line arguments
          (loop (cdr rem))])]))

  ;; Execute actions in sequential order
  (for ([act actions])
    (cond
      [(eq? (car act) 'eval)
       (when log-file
         (log-write log-file (format "[~a] IN: ~a\n" (date->string (current-date) #t) (cdr act))))
       (define res (eval-smart (cdr act)))
       (define res-str (nl->string res #t))
       (displayln res-str)
       (flush-output)
       (when (and log-file log-all?)
         (log-write log-file (format "[~a] OUT: ~a\n" (date->string (current-date) #t) res-str)))]
      [(eq? (car act) 'load)
       (define path-or-url (cdr act))
       (when log-file
         (log-write log-file (format "[~a] LOAD: ~a\n" (date->string (current-date) #t) path-or-url)))
       (define source
         (cond
           [(or (string-prefix? path-or-url "http://")
                (string-prefix? path-or-url "https://")
                (string-prefix? path-or-url "file://"))
            (define body (perform-http-request #"GET" path-or-url #f "" #f #f))
            (if (string-prefix? body "ERR:")
                (error 'load "cannot load URL ~a: ~a" path-or-url body)
                body)]
           [(file-exists? path-or-url)
            (file->string path-or-url)]
           [else
            (error 'main "cannot open script file: ~a" path-or-url)]))
       (eval-smart source)]))

  ;; Server mode check
  (if server-port
      (run-server #:port server-port
                  #:daemon? daemon-mode?
                  #:http-only? http-only?
                  #:http-safe? http-safe?
                  #:ipv6? ipv6?
                  #:timeout-us timeout-us
                  #:log-file log-file
                  #:log-all? log-all?
                  #:no-prompt? no-prompt?)

      ;; Interactive REPL mode
      (cond
        ;; If no actions executed, launch REPL
        [(null? actions)
         (run-repl #:prompt? (or force-prompt? (not no-prompt?))
                   #:banner? (not no-prompt?)
                   #:log-file log-file
                   #:log-all? log-all?)]

        ;; Actions were executed, drop to REPL only if force-prompt
        [force-prompt?
         (run-repl #:prompt? #t
                   #:banner? #f
                   #:log-file log-file
                   #:log-all? log-all?)]

        [else (void)])))
