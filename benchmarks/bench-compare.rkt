#lang racket/base

(require racket/string
         racket/format
         racket/system
         racket/port
         "../nl-types.rkt"
         "../nl-reader.rkt"
         "../nl-eval.rkt"
         "../nl-builtins.rkt"
         "../nl-transpile.rkt")

(define orig-newlisp-path
  (let ([cand (or (find-executable-path "newlisp.exe")
                  (find-executable-path "newlisp"))])
    (cond
      [(and cand (file-exists? cand)) cand]
      [(file-exists? "C:\\Program Files (x86)\\newlisp\\newlisp.exe") "C:\\Program Files (x86)\\newlisp\\newlisp.exe"]
      [(file-exists? "C:\\Program Files\\newlisp\\newlisp.exe") "C:\\Program Files\\newlisp\\newlisp.exe"]
      [else #f])))

(define (run-orig-newlisp code-str)
  (if (not orig-newlisp-path)
      (values #f #f)
      (let ()
        (define-values (sp stdout stdin stderr)
          (subprocess #f #f #f orig-newlisp-path "-e" (format "(println (time (begin ~a)))" code-str)))
        (define str (port->string stdout))
        (subprocess-wait sp)
        (close-input-port stdout)
        (close-output-port stdin)
        (close-input-port stderr)
        (define lines (string-split (string-trim str) "\n"))
        (define time-val (and (pair? lines) (string->number (string-trim (car lines)))))
        (values time-val time-val))))

(define (measure thunk)
  (define start (current-inexact-milliseconds))
  (define res (thunk))
  (define elapsed (- (current-inexact-milliseconds) start))
  (values res elapsed))

(define (run-benchmark name code-str)
  (printf "\n------------------------------------------------------------\n")
  (printf " Benchmark: ~a\n" name)
  (printf "------------------------------------------------------------\n")

  ;; Warm-up / compile
  (define compiled-thunk (compile-nl-body (nl-read-all code-str (lambda (s) (find-or-create-symbol s (current-context))))))

  ;; 1. Original C newLISP run (if available)
  (define orig-time
    (and orig-newlisp-path
         (begin
           (printf " [Original C newLISP] Running native binary...\n")
           (let-values ([(_ t) (run-orig-newlisp code-str)])
             (when t
               (printf "   Time: ~a ms\n" (~r t #:precision '(= 2))))
             t))))

  ;; 2. Transpiled & Compiled run (Racket JIT)
  (printf " [Racket Transpiled]  Running compiled bytecode...\n")
  (define-values (res-compiled time-compiled)
    (measure (lambda () (compiled-thunk))))
  (printf "   Time: ~a ms | Result: ~a\n" (~r time-compiled #:precision '(= 2)) (nl->string res-compiled #t))

  ;; Comparison
  (when orig-time
    (define vs-orig (/ orig-time (max 0.001 time-compiled)))
    (if (>= vs-orig 1.0)
        (printf " >> Transpiled vs Original C newLISP: ~ax FASTER!\n" (~r vs-orig #:precision '(= 1)))
        (printf " >> Transpiled vs Original C newLISP: ~ax relative speed (~a ms vs ~a ms)\n"
                (~r vs-orig #:precision '(= 2))
                (~r time-compiled #:precision '(= 1))
                (~r orig-time #:precision '(= 1))))))

(printf "============================================================\n")
(printf "     newLISP: Racket Transpiler vs Original C newLISP       \n")
(printf "============================================================\n")
(when orig-newlisp-path
  (printf " Original newLISP detected at: ~a\n" orig-newlisp-path))

;; Benchmark 1: Recursive Fibonacci (fib 30)
(run-benchmark
 "1. Recursive Fibonacci (fib 30)"
 "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)")

;; Benchmark 2: Tight Loop Place Mutation (dotimes with ++ 1,000,000 times)
(run-benchmark
 "2. Tight Loop Place Mutation (dotimes with ++ 1,000,000 iterations)"
 "(begin (setq sum 0) (dotimes (i 1000000) (++ sum i)) sum)")

;; Benchmark 3: List Map & Filter (100,000 elements)
(run-benchmark
 "3. List Operations: sequence, map, filter (100,000 elements)"
 "(begin (setq nums (sequence 1 100000)) (length (filter (fn (x) (= (% x 2) 0)) (map (fn (x) (+ x 1)) nums))))")

;; Benchmark 4: FOOP Method Dispatch (100,000 calls)
(run-benchmark
 "4. FOOP Method Dispatch (100,000 method invocations)"
 "(begin (new Class 'Counter) (define (Counter:inc-by d) (inc (self 1) d)) (setq c (Counter 0)) (dotimes (i 100000) (:inc-by c 1)) c)")

(printf "\n============================================================\n")
(printf " Benchmark Complete!\n")
(printf "============================================================\n\n")
