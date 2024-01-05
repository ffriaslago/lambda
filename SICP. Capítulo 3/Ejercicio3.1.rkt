#lang racket

(define (make-accumulator initial-value)
  (lambda (amount)
    (set! initial-value (+ initial-value amount))
    initial-value))

(define A (make-accumulator 5))

(A 10) ; 15

(A 10) ; 25