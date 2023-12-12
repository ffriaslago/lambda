#lang sicp

; Focusing on integers stream (1 2 3 4 5 6)
; 1 3(1+2) 6(1+2+3) 10(1+2+3+4) 15(1+2+3+4+5)

(define (partial-sums s)
  (cons-stream (stream-car s) ; 1
               (add-streams (stream-cdr s) ; 2 3 4 5 6
                            (partial-sums s)))) ; 1 3 6 10 15

;   2 3  4   5   6 ... = (stream-cdr integers)
; + 1 3  6  10  15 ... = (partial-sums integers)
; ----------------------
; 1 3 6 10  15  21 ... = (partial-sums integers)
                            