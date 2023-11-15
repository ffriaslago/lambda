#lang racket

; I understand the exercise as a generic procedure, not as a coercion, although I am aware that it can be designed 

; General definition

(define (raise x) (apply-generic 'raise x))

; From integer to rational

(put 'raise '(integer) (lambda (x) (make-rational x 1)))

; From rational to real

(put 'raise '(rational) (lambda (x) (make-real (* 1.0 (/ (numer x) (denom x)))))) ; Assuming make-real is just a procedure to put the 'real tag to the number

; From real to complex

(put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))