#lang racket

(define (fast-* a b)
  (cond ((= b 0) b)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(fast-* 3 4) ; 12

(fast-* 3 5) ; 15

(fast-* 4 8) ; 32



