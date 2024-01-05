#lang racket

(define dx 0.00001)

(define (smooth g)
  (lambda (x)
    (/ (+ (g (- x dx)) (g x) (g (+ x dx)))
       3)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        (compose f identity)
        (compose f (repeated f (- n 1)))))

(define (square x) (* x x))

(define (cube x) (* x x x))

((repeated (smooth square) 2) 5) ; 625.0000000033998

((repeated (smooth cube) 2) 5) ; 1953125.0000468995