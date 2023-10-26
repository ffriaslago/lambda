#lang racket

; Procedure of Exercise 1.26, turns the process into a O(n) one (n would be the number it is checked whether is prime or not)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

;Procedure of textbook, O(log n ) process

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

; The difference is that in the Louis procedure, instead of calculating (expmod base (/ exp 2) m) once and then squaring, it is calculated twice.
; As this is a recursive call, the process turns into a O[log(2^n)] process and O[log(2^n)]=O[n·log(2)]=O(n) as Eva said
