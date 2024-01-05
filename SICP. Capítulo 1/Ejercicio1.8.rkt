#lang racket

(define (cubic-sqrt x)
  (cubic-sqrt-iter 1.0 x))

(define (cubic-sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubic-sqrt-iter (improve guess x) x)))

(define (improve y x)
  (/
   (+
    (/ x
       (* y y))
    (* 2 y))
   3))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(cubic-sqrt 2) ; 1.2599

(cubic-sqrt 5) ; 1.7101

(cubic-sqrt 27) ; 3.0000

