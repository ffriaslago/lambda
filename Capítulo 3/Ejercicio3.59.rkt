#lang sicp

; Section a.

(define ones (cons-stream 1 ones)) ; From the book

(define (integrate-series s)
  (let ((denominators (stream-map / ones integers))) ; First it is created a stream with the fractions that will multiply the coefficients
        (stream-map * denominators s))) ; Lastly it is created the final stream with the factors applied to each coefficient

; Section b.

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))