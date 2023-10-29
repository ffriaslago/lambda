#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (inc n)
  (+ n 1))

((double inc) 2) ; 4
    
(((double (double double)) inc) 5) ; 21. (double inc) -> +2 (because it is (inc (inc n))). inc it is doubled eight times, i.e, it adds 16