#lang racket

; As this exercise has the same structure as previous Exercise 2.79, here there will be only the new lines

(define (zero? x) (apply-generic 'zero? x))

; Ordinary numbers

(put 'zero? '(scheme-number)
       (lambda (x) (tag (= x 0))))

; Rational numbers

(put 'zero? '(rational)
       (lambda (x) (tag (= (numer x) 0))))

; Complex numbers

(put 'zero '(complex)
       (lambda (z1) (tag (and (= (real-part z1) 0) (= (imag-part z1) 0)))))