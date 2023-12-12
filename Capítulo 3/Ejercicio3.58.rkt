#lang sicp

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den) ; quotient is a primitive procedure that returns the integer quotient of two integers
   (expand (remainder (* num radix) den) den radix)))

; (expand 1 7 10)
;
; (cons-stream
;  (quotient 10 7) -> 1
;  (expand 3 7 10) -> 4 (expand 2 7 10) -> 2 (expand 6 7 10) -> 8 (expand 4 7 10) -> 5 (expand 5 7 10) -> 7 (expand 1 7 10) 
;
; (expand 1 7 10) -> 1 4 2 8 5 7 1 4 2 8 5 7 ...


; (expand 3 8 10)
; -> 3 (expand 6 8 10) -> 7 (expand 4 8 10) -> 5 (expand 0 8 10) -> 0 (expand 0 8 10) -> 0 (expand 0 8 10) -> 0 (expand 0 8 10)
;
; (expand 3 8 10)-> 3 7 5 0 0 0 ...

; Once done these examples is easy to see that 1/7=.142875142875 and 3/8*10=.375

; From these results, it is concluded that the expand procedure calculates the value of dividing num by den in base radix