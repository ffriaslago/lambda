#lang racket

(define (fast-* a b)
  (fast-*-iter 0 a b))

(define (fast-*-iter aux a b)
  (cond ((= b 1) aux)
        ((even? b) (double (fast-*-iter (+ aux a) a (halve b))))
        (else (fast-*-iter (+ aux a) a (- b 1)))))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(fast-* 3 5) ; 15

(fast-* 20 6) ; 100 :(

(fast-* 20 7) ; 100 :(