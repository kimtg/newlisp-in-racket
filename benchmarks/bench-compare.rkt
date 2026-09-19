#lang racket/base

(require racket/string
         racket/format
         "../nl-types.rkt"
         "../nl-reader.rkt"
         "../nl-eval.rkt"
         "../nl-builtins.rkt"
         "../nl-transpile.rkt")

(define (eval-interp code-str)
  (define exprs
    (nl-read-all code-str (lambda (s) (find-or-create-symbol s (current-context)))))
  (eval-body exprs))

(define (measure thunk)
  (define start (current-inexact-milliseconds))
  (define res (thunk))
  (define elapsed (- (current-inexact-milliseconds) start))
  (values res elapsed))

(define (run-benchmark name code-str)
  (printf "\n------------------------------------------------------------\n")
  (printf " Benchmark: ~a\n" name)
  (printf "------------------------------------------------------------\n")

  ;; Warm-up / compile first
  (define compiled-thunk (compile-nl-body (nl-read-all code-str (lambda (s) (find-or-create-symbol s (current-context))))))

  ;; 1. Interpreted run
  (printf " [Interpreted] Running tree-walker...\n")
  (define-values (res-interp time-interp)
    (measure (lambda () (eval-interp code-str))))
  (printf "   Time: ~a ms | Result: ~a\n" (~r time-interp #:precision '(= 2)) (nl->string res-interp #t))

  ;; 2. Transpiled & Compiled run
  (printf " [Compiled]   Running Racket transpiled code...\n")
  (define-values (res-compiled time-compiled)
    (measure (lambda () (compiled-thunk))))
  (printf "   Time: ~a ms | Result: ~a\n" (~r time-compiled #:precision '(= 2)) (nl->string res-compiled #t))

  ;; Speedup calculation
  (define speedup (/ time-interp (max 0.001 time-compiled)))
  (printf " >> Speedup: ~ax FASTER!\n" (~r speedup #:precision '(= 1))))

(printf "============================================================\n")
(printf "        newLISP: Interpreted vs Transpiled Benchmark        \n")
(printf "============================================================\n")

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
