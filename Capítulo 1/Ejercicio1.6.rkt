#lang racket

(define (new-if predicate 
                then-clause 
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ; 5

(new-if (= 1 1) 0 5) ; 0

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y) 
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2) ; Interactions disabled; out of memory

; There is an INFINITE LOOP. new-if is not a specia form, it is a function, i.e., it evaluates the subexpressions before the procedure is applied.
; Then, it iterates sqrt-iter and the good-enough? function does not serve as a stop.