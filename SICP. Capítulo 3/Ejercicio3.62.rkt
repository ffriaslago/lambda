#lang sicp

; invert-unit-series only work for series with its constant term being 1

(define (div-series s1 s2)
  (let ((constant-term-s2 (stream-car s2)))
    (if (= 0 constant-term-s2)
        (error "Divisor series has a zero constant term")
        (mul-series (scale-stream s1 constant-term-s2)
                    (invert-unit-series (scale-stream s2 (/ 1 constant-term-s2)))))))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define tan-series (div-series sine-series cosine-series))