#lang racket

(define (fast-* a b)
  (fast-*-iter 0 a b))

(define (fast-*-iter aux a b)
  (cond ((= b 0) aux)
        ((even? b) (fast-*-iter aux (double a) (halve b)))
        (else (fast-*-iter (+ aux a) a (- b 1) ))))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(fast-* 3 12) ; 36

(fast-* 20 6) ; 120

(fast-* 20 7) ; 140 

(fast-* 56 49) ; 2744