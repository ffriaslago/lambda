#lang sicp

(define (stream-limit s tol)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tol)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) tol)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))