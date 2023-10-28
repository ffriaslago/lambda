#lang racket

(define (f g) (g 2))

(define (square x) (* x x))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)

; application: not a procedure;
; expected a procedure that can be applied to arguments given: 2

; procedure f does take as arguments procedures where 2 is an adequate argument
; (f f) would mean (f 2), what would mean (2 2) which has no sense
; The error message tries to explain this fact