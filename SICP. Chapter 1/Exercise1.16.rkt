#lang racket

(define (fast-exp b n)
  (fast-exp-iter 1 b n))

(define (fast-exp-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-exp-iter a (* b b) (/ n 2))) ; even? is imported from racket
        (else (fast-exp-iter (* a b) (* b b) (/ (- n 1) 2)))))

(fast-exp 2 3) ; 8

(fast-exp 5 6) ; 15625