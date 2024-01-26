#lang racket

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x) (* x x))

(smallest-divisor 199) ; Result: 199. 199 is a prime number

(smallest-divisor 1999) ; Result: 1999. 1999 is a prime number.

(smallest-divisor 19999) ; Result: 7. 19999 can be factorised as 7·2857.