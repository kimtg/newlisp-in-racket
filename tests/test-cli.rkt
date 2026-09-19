#lang racket/base

(require racket/file
         racket/string
         racket/list
         racket/system
         racket/port
         racket/tcp
         "../nl-types.rkt"
         "../nl-eval.rkt"
         "../nl-builtins.rkt"
         "../nl-cli.rkt")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! pass-count (+ pass-count 1))
        (printf "[PASS] ~a\n" name))
      (begin
        (set! fail-count (+ fail-count 1))
        (printf "[FAIL] ~a\n" name)
        (printf "  Expected: ~s\n" expected)
        (printf "  Actual:   ~s\n" actual))))

;; Helper to run CLI in-process with intercepted exit and captured output
(define (call-cli args [in-str ""])
  (define out (open-output-string))
  (define in (open-input-string in-str))
  (define exit-prompt-tag (make-continuation-prompt-tag 'cli-exit))
  (call-with-continuation-prompt
   (lambda ()
     (parameterize ([current-output-port out]
                    [current-error-port out]
                    [current-input-port in]
                    [exit-handler (lambda (v) (abort-current-continuation exit-prompt-tag v))])
       (run-cli args)))
   exit-prompt-tag
   (lambda (v) v))
  (get-output-string out))

(printf "====================================================\n")
(printf "      newLISP CLI & Startup Options Test Suite      \n")
(printf "====================================================\n\n")

;; 1. Help summary (-h)
(define help-out (call-cli '("-h")))
(test "-h outputs help summary" #t (string-contains? help-out "-h this help (no init.lsp)"))
(test "-h lists -n option" #t (string-contains? help-out "-n no init.lsp (must be first)"))
(test "-h lists -x option" #t (string-contains? help-out "-x <source> <target> link (no init.lsp)"))
(test "-h lists -http-safe option" #t (string-contains? help-out "-http-safe safe mode"))

;; 2. Version (-v)
(define ver-out (call-cli '("-v")))
(test "-v outputs version banner" #t (string-contains? ver-out "newLISP v.10.7.6 [Racket]"))

;; 3. Direct execution (-e)
(define e-out1 (string-trim (call-cli '("-e" "(+ 10 20 30)"))))
(test "-e single expression" "60" e-out1)

(define e-out2 (string-split (string-trim (call-cli '("-e" "(set 'a 40)" "-e" "(+ a 2)"))) "\n"))
(test "-e multiple expressions" '("40" "42") (map string-trim e-out2))

;; 4. Stack size (-s) and Cell memory (-m)
(define s-out (string-trim (call-cli '("-s" "5432" "-e" "(sys-info 5)"))))
(test "-s sets stack size in sys-info" "5432" s-out)

(define s-att-out (string-trim (call-cli '("-s6543" "-e" "(sys-info 5)"))))
(test "attached -s sets stack size" "6543" s-att-out)

(define m-out (string-trim (call-cli '("-m" "16" "-e" "(sys-info 1)"))))
(test "-m sets max cells (16 * 65536)" "1048576" m-out)

(define m-att-out (string-trim (call-cli '("-m32" "-e" "(sys-info 1)"))))
(test "attached -m sets max cells" "2097152" m-att-out)

;; 5. main-args and $main-args
(define args-out (string-trim (call-cli '("-e" "(println (main-args 0)) (println (main-args -1)) (println (main-args 999)) (exit)" "foo" "bar"))))
(define args-lines (map string-trim (string-split args-out "\n")))
(test "main-args 0 is program name" #t (> (string-length (car args-lines)) 0))
(test "main-args -1 is last arg" "bar" (cadr args-lines))
(test "main-args out of range returns nil" "nil" (caddr args-lines))

;; 6. Working directory (-w)
(define orig-dir (current-directory))
(define w-out (string-trim (call-cli '("-w" "tests" "-e" "(file? {test-all.rkt})"))))
(test "-w changes working directory" "true" w-out)
(current-directory orig-dir)

;; 7. -c silent mode and [cmd]...[/cmd]
(define c-multiline-in "[cmd]\n(define (cube x) (* x x x))\n(cube 3)\n[/cmd]\n(exit)\n")
(define c-out (string-trim (call-cli '("-c") c-multiline-in)))
(test "-c multiline [cmd] evaluation" "(lambda (x) (* x x x))\n27" (string-replace c-out "\r\n" "\n"))

;; 8. Logging (-l and -L)
(define log-test-file "test-cli-log.txt")
(when (file-exists? log-test-file) (delete-file log-test-file))
(call-cli `("-L" ,log-test-file "-e" "(+ 77 33)"))
(test "log file created with -L" #t (file-exists? log-test-file))
(when (file-exists? log-test-file)
  (define log-content (file->string log-test-file))
  (test "log file contains output" #t (string-contains? log-content "110"))
  (delete-file log-test-file))

;; 9. Standalone executable linking (-x)
(define test-script-path "test-temp-script.lsp")
(define test-exe-path "test-temp-uppercase.exe")
(with-output-to-file test-script-path
  (lambda ()
    (printf "(println (upper-case (main-args 1))) (exit)\n"))
  #:exists 'replace)

;; Create linked executable using link-executable directly
(define exit-link-tag (make-continuation-prompt-tag 'link-exit))
(call-with-continuation-prompt
 (lambda ()
   (parameterize ([exit-handler (lambda (v) (abort-current-continuation exit-link-tag v))])
     (link-executable test-script-path test-exe-path)))
 exit-link-tag
 (lambda (v) v))
(test "-x creates target file" #t (file-exists? test-exe-path))

(when (file-exists? test-exe-path)
  ;; Verify get-embedded-source extracts the exact script payload
  (define extracted-code (get-embedded-source (string->path test-exe-path)))
  (test "extracted embedded code matches source"
        "(println (upper-case (main-args 1))) (exit)\n"
        extracted-code)

  ;; Run standalone executable via subprocess
  (define-values (sp stdout stdin stderr)
    (subprocess #f #f #f (path->complete-path test-exe-path) "convert me to uppercase"))
  (close-output-port stdin)
  (define linked-run-out (string-trim (port->string stdout)))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait sp)
  (test "linked executable runs and prints expected output" "CONVERT ME TO UPPERCASE" linked-run-out)
  (sleep 0.2)
  (with-handlers ([exn:fail? void]) (delete-file test-exe-path)))

(when (file-exists? test-script-path)
  (with-handlers ([exn:fail? void]) (delete-file test-script-path)))

;; 10. Server mode (HTTP and TCP Socket)
(define srv-port 19890)
(define srv-th
  (thread
   (lambda ()
     (run-server #:port srv-port
                 #:daemon? #t
                 #:http-only? #f
                 #:http-safe? #t
                 #:no-prompt? #t))))
(sleep 0.5)

;; 10a. Test HTTP request on server
(define-values (http-cin http-cout) (tcp-connect "127.0.0.1" srv-port))
(display "GET /demo.lsp HTTP/1.0\r\n\r\n" http-cout)
(flush-output http-cout)
(define http-resp (port->string http-cin))
(close-input-port http-cin)
(close-output-port http-cout)
(test "HTTP server returns 200 OK" #t (string-contains? http-resp "200 OK"))
(test "HTTP server Content-Type header" #t (string-contains? http-resp "Content-Type: text/plain"))

;; 10b. Test HTTP 404 on missing file
(define-values (http-cin404 http-cout404) (tcp-connect "127.0.0.1" srv-port))
(display "GET /nonexistent-file.xyz HTTP/1.0\r\n\r\n" http-cout404)
(flush-output http-cout404)
(define http-resp404 (port->string http-cin404))
(close-input-port http-cin404)
(close-output-port http-cout404)
(test "HTTP server returns 404 on missing file" #t (string-contains? http-resp404 "404 Not Found"))

;; 10c. Test HTTP 403 on safe mode traversal
(define-values (http-cin403 http-cout403) (tcp-connect "127.0.0.1" srv-port))
(display "GET /../secret.txt HTTP/1.0\r\n\r\n" http-cout403)
(flush-output http-cout403)
(define http-resp403 (port->string http-cin403))
(close-input-port http-cin403)
(close-output-port http-cout403)
(test "HTTP safe mode blocks path traversal" #t (string-contains? http-resp403 "403 Forbidden"))

;; 10d. Test TCP Lisp command evaluation
(define-values (lisp-cin lisp-cout) (tcp-connect "127.0.0.1" srv-port))
(display "(+ 123 456)\n" lisp-cout)
(flush-output lisp-cout)
(define lisp-line (read-line lisp-cin 'any))
(display "(exit)\n" lisp-cout)
(flush-output lisp-cout)
(close-input-port lisp-cin)
(close-output-port lisp-cout)
(test "TCP server evaluates Lisp expression" "579" (string-trim lisp-line))

(kill-thread srv-th)

(printf "\n====================================================\n")
(printf "CLI Tests Total:  ~a\n" test-count)
(printf "CLI Tests Passed: ~a\n" pass-count)
(printf "CLI Tests Failed: ~a\n" fail-count)
(printf "====================================================\n")

(if (= fail-count 0) (exit 0) (exit 1))
