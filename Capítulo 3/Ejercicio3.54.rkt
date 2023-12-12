#lang sicp

; Completely analogous to add-streams

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

; Goal: nth element (counting from 0) of factorials is n+1 factorial.
; So, stream shloud be
; 1 1(*1) 2(*2) 6(*3) 24(*4) 120(*5) 720(*6) ...

;   1 2 3  4   5   6 ... = integers
; * 1 1 2  6  24 120 ... = factorials
; ----------------------
; 1 1 2 6 24 120 720 ... = factorials