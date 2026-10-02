#lang racket/base

(require "nl-cli.rkt")

(provide main
         (all-from-out "nl-cli.rkt"))

(define (main)
  (void (run-cli)))

(module+ main
  (main))
