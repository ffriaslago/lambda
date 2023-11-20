#lang racket

(define f
  (let ((initial-value 0))
  (lambda (amount)
    (set! initial-value (- amount initial-value))
    (- amount initial-value))))

;(+ (f 0) (f 1)) ; 0

; (f 0) sets initial-value to 0, returns 0 [amount(0)-initial-value(0)]
; (f 1) sets initial-value to 1, returns 0 [amount(1)-initial-value(1)]

;(+ (f 1) (f 0)) ; 1

; (f 1) sets initial-value to 1, returns 0 [amount(1)-initial-value(1)]
; (f 0) sets initial-value to -1, returns 1 [amount(0)-initial-value(-1)

