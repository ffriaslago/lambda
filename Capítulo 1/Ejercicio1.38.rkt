#lang racket

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (d i)
  (if (= (remainder i 3) 2)
      (+ 2 (* 2 (floor (/ i 3))))
      1))

(+ 2 (cont-frac (lambda (i) 1.0) d 10)) ; 2.7182818284590455

