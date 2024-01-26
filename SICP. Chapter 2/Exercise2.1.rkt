#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; With this construction of gcd
; If a&b positive -> gcd positive
; If a negative & b positive -> gcd negative
; If a positive & b negative -> gcd positive
; If a&b negative -> gcd negative
; So, the sign of a is the sign of the gcd

; If n&d positive -> no change
; If n&d negative -> no change
; If n pos & d neg or other way around -> change

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (cons (/ n (- g)) (/ d (- g)))
        (cons (/ n g) (/ d g)))))

; Other procedures

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-third (make-rat -1 -3))

(print-rat one-third)