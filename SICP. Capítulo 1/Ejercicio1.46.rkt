#lang racket

(define (iterative-improve good-enough? improve-procedure)
  (define (iter-improve guess)
    (if (good-enough? guess)
        guess
        (iter-improve (improve-procedure guess))))
  (lambda (x) (iter-improve x)))

; sqrt

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess) (< (abs (- (square guess) x)) 0.001))
  (define (improve-procedure guess) (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-procedure) 1.0))

(sqrt 2) ; 1.4142156862745097 Fine!

; fixed-point

(define (fixed-point f first-guess)
  (define (good-enough? guess) (< (abs (- guess (f guess))) 0.00001))
  (define (improve-procedure guess) (average guess (f guess)))
  ((iterative-improve good-enough? improve-procedure) first-guess))

(fixed-point cos 1.0) ; 0.7390885809390573 Fine!