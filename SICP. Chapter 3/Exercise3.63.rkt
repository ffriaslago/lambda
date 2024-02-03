#lang sicp

; Original procedure

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses))) ; No new stream is created in this procedure
  guesses)

; Exercise 3.63 procedure

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                 (sqrt-improve guess x))
               (sqrt-stream x)))) ; Here it is created a new stream with no storaged values, there is redundant calculation

; Whithout the memoization both procedures would need to recalculate everything, so both would be inefficient