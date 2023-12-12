#lang sicp

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) ; First coefficients of each series
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) ; First coefficient of series 1 multiplying all coefficients of series 2. We are done with a_0 of series 1
                            (mul-series (stream-cdr s1) s2)))) ; Finally, recursive call to the whole multiplication of both series but the first element of the first one