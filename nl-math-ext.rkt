#lang racket/base

(require racket/math
         racket/list
         racket/vector
         "nl-types.rkt"
         "nl-eval.rkt")

(provide (all-defined-out))

;; -------------------------------------------------------------------
;; Matrix Functions
;; -------------------------------------------------------------------

(define (nl-mat args)
  (define func (car args))
  (define rows (cadr args))
  (define cols (caddr args))
  (define extra-args (cdddr args))
  (for/list ([r (in-range rows)])
    (for/list ([c (in-range cols)])
      (if (or (nl-lambda? func) (nl-primitive? func))
          (nl-eval (list* func r c extra-args))
          func))))

(define (nl-det args)
  (define mat (car args))
  (define n (length mat))
  (when (or (= n 0) (not (= (length (car mat)) n)))
    (error 'det "expected square matrix"))
  ;; Convert to 2D vector for in-place Gaussian elimination
  (define a (for/vector ([row mat])
              (for/vector ([v row]) (exact->inexact v))))
  (define det 1.0)
  (let/ec return
    (for ([i (in-range n)])
      ;; Pivot
      (define pivot-row
        (for/fold ([max-r i]) ([r (in-range (+ i 1) n)])
          (if (> (abs (vector-ref (vector-ref a r) i))
                 (abs (vector-ref (vector-ref a max-r) i)))
              r
              max-r)))
      (when (= (vector-ref (vector-ref a pivot-row) i) 0.0)
        (return 0.0))
      (unless (= pivot-row i)
        ;; swap rows
        (define tmp (vector-ref a i))
        (vector-set! a i (vector-ref a pivot-row))
        (vector-set! a pivot-row tmp)
        (set! det (- det)))
      (define pivot (vector-ref (vector-ref a i) i))
      (set! det (* det pivot))
      (for ([r (in-range (+ i 1) n)])
        (define factor (/ (vector-ref (vector-ref a r) i) pivot))
        (for ([c (in-range i n)])
          (vector-set! (vector-ref a r) c
                       (- (vector-ref (vector-ref a r) c)
                          (* factor (vector-ref (vector-ref a i) c)))))))
    det))

(define (nl-invert args)
  (define mat (car args))
  (define n (length mat))
  (when (or (= n 0) (not (= (length (car mat)) n)))
    (error 'invert "expected square matrix"))
  (define a (for/vector ([row mat])
              (for/vector ([v row]) (exact->inexact v))))
  (define inv (for/vector ([r (in-range n)])
                (for/vector ([c (in-range n)])
                  (if (= r c) 1.0 0.0))))
  (for ([i (in-range n)])
    ;; Find pivot
    (define pivot-row
      (for/fold ([max-r i]) ([r (in-range (+ i 1) n)])
        (if (> (abs (vector-ref (vector-ref a r) i))
               (abs (vector-ref (vector-ref a max-r) i)))
            r
            max-r)))
    (define pivot (vector-ref (vector-ref a pivot-row) i))
    (when (< (abs pivot) 1e-15)
      (error 'invert "matrix is singular"))
    (unless (= pivot-row i)
      (define tmp-a (vector-ref a i))
      (vector-set! a i (vector-ref a pivot-row))
      (vector-set! a pivot-row tmp-a)
      (define tmp-inv (vector-ref inv i))
      (vector-set! inv i (vector-ref inv pivot-row))
      (vector-set! inv pivot-row tmp-inv))
    (set! pivot (vector-ref (vector-ref a i) i))
    ;; Normalize row i
    (for ([c (in-range n)])
      (vector-set! (vector-ref a i) c (/ (vector-ref (vector-ref a i) c) pivot))
      (vector-set! (vector-ref inv i) c (/ (vector-ref (vector-ref inv i) c) pivot)))
    ;; Eliminate other rows
    (for ([r (in-range n)])
      (unless (= r i)
        (define factor (vector-ref (vector-ref a r) i))
        (for ([c (in-range n)])
          (vector-set! (vector-ref a r) c
                       (- (vector-ref (vector-ref a r) c)
                          (* factor (vector-ref (vector-ref a i) c))))
          (vector-set! (vector-ref inv r) c
                       (- (vector-ref (vector-ref inv r) c)
                          (* factor (vector-ref (vector-ref inv i) c))))))))
  (for/list ([r (in-range n)])
    (for/list ([c (in-range n)])
      (vector-ref (vector-ref inv r) c))))

(define (nl-multiply args)
  (define m1 (car args))
  (define m2 (cadr args))
  (cond
    ;; Matrix x Matrix
    [(and (pair? m1) (pair? (car m1)) (pair? m2) (pair? (car m2)))
     (define r1 (length m1))
     (define c1 (length (car m1)))
     (define r2 (length m2))
     (define c2 (length (car m2)))
     (unless (= c1 r2)
       (error 'multiply "incompatible matrix dimensions: ~ax~a and ~ax~a" r1 c1 r2 c2))
     (for/list ([i (in-range r1)])
       (for/list ([j (in-range c2)])
         (for/fold ([sum 0.0]) ([k (in-range c1)])
           (+ sum (* (list-ref (list-ref m1 i) k)
                     (list-ref (list-ref m2 k) j))))))]
    ;; Matrix x Vector
    [(and (pair? m1) (pair? (car m1)) (pair? m2) (not (pair? (car m2))))
     (for/list ([row m1])
       (for/fold ([sum 0.0]) ([a row] [b m2])
         (+ sum (* a b))))]
    ;; Vector x Vector (Dot product)
    [(and (pair? m1) (not (pair? (car m1))) (pair? m2) (not (pair? (car m2))))
     (for/fold ([sum 0.0]) ([a m1] [b m2])
       (+ sum (* a b)))]
    [else
     (error 'multiply "invalid arguments to multiply: ~a ~a" m1 m2)]))

;; -------------------------------------------------------------------
;; Special Math Functions
;; -------------------------------------------------------------------

(define (nl-factor args)
  (define n (abs (car args)))
  (if (<= n 1)
      (list n)
      (let loop ([d 2] [rem n] [acc '()])
        (cond
          [(= rem 1) (reverse acc)]
          [(> (* d d) rem) (reverse (cons rem acc))]
          [(zero? (modulo rem d)) (loop d (quotient rem d) (cons d acc))]
          [(= d 2) (loop 3 rem acc)]
          [else (loop (+ d 2) rem acc)]))))

(define (nl-binomial args)
  (define n (car args))
  (define k (cadr args))
  (define p (if (pair? (cddr args)) (caddr args) #f))
  ;; C(n, k)
  (define coeff
    (if (or (< k 0) (> k n))
        0
        (let loop ([i 1] [c 1])
          (if (> i k)
              c
              (loop (+ i 1) (/ (* c (- (+ n 1) i)) i))))))
  (if p
      (* coeff (expt p k) (expt (- 1.0 p) (- n k)))
      coeff))

(define (nl-series args)
  (define start (car args))
  (define step (cadr args))
  (define count (caddr args))
  (for/list ([i (in-range count)])
    (+ start (* i step))))

(define (nl-ssq args)
  (define lst (car args))
  (for/fold ([sum 0]) ([x lst])
    (+ sum (* x x))))

;; Lanczos approximation for ln(gamma(x))
(define (nl-gammaln args)
  (define x (exact->inexact (car args)))
  (define cof '(76.18009172947146 -86.50532032941677 24.01409824083091
                -1.231739572450155 0.1208650973866179e-2 -0.5395239384953e-5))
  (define y x)
  (define tmp (+ x 5.5))
  (set! tmp (- tmp (* (+ x 0.5) (log tmp))))
  (define ser
    (for/fold ([s 1.000000000190015] [c cof])
              ([coeff cof])
      (set! y (+ y 1.0))
      (+ s (/ coeff y))))
  (- (log (* 2.5066282746310005 (/ ser x))) tmp))

(define (nl-beta args)
  (define a (car args))
  (define b (cadr args))
  (exp (- (+ (nl-gammaln (list a)) (nl-gammaln (list b)))
          (nl-gammaln (list (+ a b))))))

;; Incomplete gamma function via series
(define (nl-gammai args)
  (define a (exact->inexact (car args)))
  (define x (exact->inexact (cadr args)))
  (if (<= x 0.0)
      0.0
      (let loop ([n 1] [sum (/ 1.0 a)] [term (/ 1.0 a)])
        (if (> n 100)
            (* (exp (- (* a (log x)) x (nl-gammaln (list a)))) sum)
            (let* ([new-term (/ (* term x) (+ a n))]
                   [new-sum (+ sum new-term)])
              (if (< (abs new-term) (* (abs new-sum) 1e-15))
                  (* (exp (- (* a (log x)) x (nl-gammaln (list a)))) new-sum)
                  (loop (+ n 1) new-sum new-term)))))))

(define (nl-betai args)
  (define x (exact->inexact (car args)))
  (define a (exact->inexact (cadr args)))
  (define b (exact->inexact (caddr args)))
  (cond
    [(<= x 0.0) 0.0]
    [(>= x 1.0) 1.0]
    [else
     ;; Continued fraction approximation
     (define bt (exp (- (+ (nl-gammaln (list (+ a b)))
                           (* a (log x))
                           (* b (log (- 1.0 x))))
                        (nl-gammaln (list a))
                        (nl-gammaln (list b)))))
     (if (< x (/ (+ a 1.0) (+ a b 2.0)))
         (/ (* bt (betacf a b x)) a)
         (- 1.0 (/ (* bt (betacf b a (- 1.0 x))) b)))]))

(define (betacf a b x)
  (define qab (+ a b))
  (define qap (+ a 1.0))
  (define qam (- a 1.0))
  (define c 1.0)
  (define d (- 1.0 (/ (* qab x) qap)))
  (when (< (abs d) 1e-30) (set! d 1e-30))
  (set! d (/ 1.0 d))
  (define h d)
  (for ([m (in-range 1 100)])
    (define m2 (* 2 m))
    (define aa (/ (* m (- b m) x) (* (+ qam m2) (+ a m2))))
    (set! d (+ 1.0 (* aa d)))
    (when (< (abs d) 1e-30) (set! d 1e-30))
    (set! c (+ 1.0 (/ aa c)))
    (when (< (abs c) 1e-30) (set! c 1e-30))
    (set! d (/ 1.0 d))
    (set! h (* h (* c d)))
    (set! aa (- (/ (* (+ a m) (+ qab m) x) (* (+ a m2) (+ qap m2)))))
    (set! d (+ 1.0 (* aa d)))
    (when (< (abs d) 1e-30) (set! d 1e-30))
    (set! c (+ 1.0 (/ aa c)))
    (when (< (abs c) 1e-30) (set! c 1e-30))
    (set! d (/ 1.0 d))
    (define del (* c d))
    (set! h (* h del)))
  h)

;; Gauss error function erf(x)
(define (nl-erf args)
  (define x (exact->inexact (car args)))
  ;; Using numerical approximation
  (define sign (if (< x 0) -1.0 1.0))
  (define ax (abs x))
  (define t (/ 1.0 (+ 1.0 (* 0.3275911 ax))))
  (define poly (+ (* 0.254829592 t)
                  (* -0.284496736 (expt t 2))
                  (* 1.421413741 (expt t 3))
                  (* -1.453152027 (expt t 4))
                  (* 1.061405429 (expt t 5))))
  (define res (- 1.0 (* poly (exp (- (* ax ax))))))
  (* sign res))

;; -------------------------------------------------------------------
;; Statistics & Distributions
;; -------------------------------------------------------------------

(define (nl-stats args)
  (define lst (car args))
  (define n (length lst))
  (if (null? lst)
      '(0 0 0 0 0)
      (let* ([sum (apply + lst)]
             [mean (/ (exact->inexact sum) n)]
             [var (if (> n 1)
                      (/ (for/fold ([s 0.0]) ([x lst]) (+ s (sqr (- x mean))))
                         (- n 1))
                      0.0)]
             [sd (sqrt var)]
             [min-v (apply min lst)]
             [max-v (apply max lst)])
        (list n mean sd min-v max-v))))

(define (nl-corr args)
  (define x (car args))
  (define y (cadr args))
  (define n (length x))
  (unless (= n (length y))
    (error 'corr "vectors must have equal length"))
  (define mx (/ (exact->inexact (apply + x)) n))
  (define my (/ (exact->inexact (apply + y)) n))
  (define cov (for/fold ([s 0.0]) ([a x] [b y]) (+ s (* (- a mx) (- b my)))))
  (define sx (sqrt (for/fold ([s 0.0]) ([a x]) (+ s (sqr (- a mx))))))
  (define sy (sqrt (for/fold ([s 0.0]) ([b y]) (+ s (sqr (- b my))))))
  (if (or (zero? sx) (zero? sy))
      0.0
      (/ cov (* sx sy))))

(define (nl-normal args)
  (define mean (if (pair? args) (car args) 0.0))
  (define sd (if (pair? (cdr args)) (cadr args) 1.0))
  ;; Box-Muller transform
  (define u1 (random))
  (define u2 (random))
  (define z (* (sqrt (* -2.0 (log (max 1e-15 u1))))
               (cos (* 2.0 pi u2))))
  (+ mean (* z sd)))

(define (nl-prob-z args)
  (define z (exact->inexact (car args)))
  (* 0.5 (+ 1.0 (nl-erf (list (/ z (sqrt 2.0)))))))

(define (nl-crit-z args)
  (define p (exact->inexact (car args)))
  ;; Rational approximation for inverse normal CDF
  (define t (if (< p 0.5) (sqrt (* -2.0 (log p))) (sqrt (* -2.0 (log (- 1.0 p))))))
  (define c0 2.515517)
  (define c1 0.802853)
  (define c2 0.010328)
  (define d1 1.432788)
  (define d2 0.189269)
  (define d3 0.001308)
  (define z (- t (/ (+ c0 (* c1 t) (* c2 (sqr t)))
                    (+ 1.0 (* d1 t) (* d2 (sqr t)) (* d3 (expt t 3))))))
  (if (< p 0.5) (- z) z))

(define (nl-prob-chi2 args)
  (define x (car args))
  (define df (cadr args))
  (nl-gammai (list (/ df 2.0) (/ x 2.0))))

(define (nl-crit-chi2 args)
  (define p (car args))
  (define df (cadr args))
  ;; Approximation using Wilson-Hilferty transformation
  (define z (nl-crit-z (list p)))
  (* df (expt (+ 1.0 (* (/ 2.0 (* 9.0 df)) z) (- (/ 2.0 (* 9.0 df)))) 3)))

(define (nl-prob-t args)
  (define t (car args))
  (define df (cadr args))
  (define x (/ df (+ df (sqr t))))
  (- 1.0 (* 0.5 (nl-betai (list x (/ df 2.0) 0.5)))))

(define (nl-crit-t args)
  (define p (car args))
  (define df (cadr args))
  ;; Hill's approximation for student-t inverse
  (define z (nl-crit-z (list p)))
  (+ z (/ (+ (expt z 3) z) (* 4.0 df))))

(define (nl-prob-f args)
  (define f (car args))
  (define df1 (cadr args))
  (define df2 (caddr args))
  (define x (/ (* df1 f) (+ (* df1 f) df2)))
  (nl-betai (list x (/ df1 2.0) (/ df2 2.0))))

(define (nl-crit-f args)
  (define p (car args))
  (define df1 (cadr args))
  (define df2 (caddr args))
  (define z (nl-crit-z (list p)))
  (define h (/ 2.0 (+ (/ 1.0 df1) (/ 1.0 df2))))
  (define lambda (/ (- (sqr z) 3.0) 6.0))
  (define w (/ (* z (sqrt (+ h lambda))) h))
  (exp (* 2.0 w)))

(define (nl-t-test args)
  (define x (car args))
  (define y (cadr args))
  (define nx (length x))
  (define ny (length y))
  (define mx (/ (exact->inexact (apply + x)) nx))
  (define my (/ (exact->inexact (apply + y)) ny))
  (define vx (/ (for/fold ([s 0.0]) ([a x]) (+ s (sqr (- a mx)))) (- nx 1)))
  (define vy (/ (for/fold ([s 0.0]) ([b y]) (+ s (sqr (- b my)))) (- ny 1)))
  (define sp (sqrt (+ (/ vx nx) (/ vy ny))))
  (define t (/ (- mx my) (max 1e-15 sp)))
  (define df (+ nx ny -2))
  (define prob (nl-prob-t (list (abs t) df)))
  (list t df prob))

;; -------------------------------------------------------------------
;; Financial Functions
;; -------------------------------------------------------------------

(define (nl-pv args)
  (define rate (exact->inexact (car args)))
  (define nper (exact->inexact (cadr args)))
  (define pmt (exact->inexact (caddr args)))
  (define fv (if (pair? (cdddr args)) (exact->inexact (cadddr args)) 0.0))
  (define type (if (and (pair? (cdddr args)) (pair? (cddddr args))) (car (cddddr args)) 0))
  (if (zero? rate)
      (- (+ (* pmt nper) fv))
      (let* ([pvif (expt (+ 1.0 rate) nper)]
             [fact (if (= type 1) (+ 1.0 rate) 1.0)])
        (- (/ (+ (* pmt fact (/ (- pvif 1.0) rate)) fv) pvif)))))

(define (nl-fv args)
  (define rate (exact->inexact (car args)))
  (define nper (exact->inexact (cadr args)))
  (define pmt (exact->inexact (caddr args)))
  (define pv (if (pair? (cdddr args)) (exact->inexact (cadddr args)) 0.0))
  (define type (if (and (pair? (cdddr args)) (pair? (cddddr args))) (car (cddddr args)) 0))
  (if (zero? rate)
      (- (+ (* pmt nper) pv))
      (let* ([pvif (expt (+ 1.0 rate) nper)]
             [fact (if (= type 1) (+ 1.0 rate) 1.0)])
        (- (+ (* pv pvif) (* pmt fact (/ (- pvif 1.0) rate)))))))

(define (nl-pmt args)
  (define rate (exact->inexact (car args)))
  (define nper (exact->inexact (cadr args)))
  (define pv (exact->inexact (caddr args)))
  (define fv (if (pair? (cdddr args)) (exact->inexact (cadddr args)) 0.0))
  (define type (if (and (pair? (cdddr args)) (pair? (cddddr args))) (car (cddddr args)) 0))
  (if (zero? rate)
      (- (/ (+ pv fv) nper))
      (let* ([pvif (expt (+ 1.0 rate) nper)]
             [fact (if (= type 1) (+ 1.0 rate) 1.0)])
        (- (/ (* rate (+ (* pv pvif) fv))
              (* fact (- pvif 1.0)))))))

(define (nl-nper args)
  (define rate (exact->inexact (car args)))
  (define pmt (exact->inexact (cadr args)))
  (define pv (exact->inexact (caddr args)))
  (define fv (if (pair? (cdddr args)) (exact->inexact (cadddr args)) 0.0))
  (define type (if (and (pair? (cdddr args)) (pair? (cddddr args))) (car (cddddr args)) 0))
  (if (zero? rate)
      (- (/ (+ pv fv) pmt))
      (let ([fact (if (= type 1) (+ 1.0 rate) 1.0)])
        (/ (log (/ (- (* pmt fact) (* fv rate))
                   (+ (* pmt fact) (* pv rate))))
           (log (+ 1.0 rate))))))

(define (nl-npv args)
  (define rate (exact->inexact (car args)))
  (define cfs (cadr args))
  (for/fold ([sum 0.0] [i 1] #:result sum) ([cf cfs])
    (values (+ sum (/ (exact->inexact cf) (expt (+ 1.0 rate) i)))
            (+ i 1))))

(define (nl-irr args)
  (define cfs (car args))
  (define guess (if (pair? (cdr args)) (exact->inexact (cadr args)) 0.1))
  ;; Newton-Raphson
  (let loop ([rate guess] [iter 0])
    (if (> iter 100)
        rate
        (let* ([npv-val
                (for/fold ([s 0.0] [i 0] #:result s) ([cf cfs])
                  (values (+ s (/ (exact->inexact cf) (expt (+ 1.0 rate) i)))
                          (+ i 1)))]
               [deriv
                (for/fold ([s 0.0] [i 0] #:result s) ([cf cfs])
                  (values (- s (/ (* i (exact->inexact cf)) (expt (+ 1.0 rate) (+ i 1))))
                          (+ i 1)))])
          (if (< (abs deriv) 1e-12)
              rate
              (let ([new-rate (- rate (/ npv-val deriv))])
                (if (< (abs (- new-rate rate)) 1e-9)
                    new-rate
                    (loop new-rate (+ iter 1)))))))))
