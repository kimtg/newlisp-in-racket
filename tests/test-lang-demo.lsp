#lang reader "../newlisp/lang/reader.rkt"
(define (fact n)
  (if (<= n 1) 1 (* n (fact (- n 1)))))
(println "Fact 6 is: " (fact 6))
(println "args: " (main-args))
