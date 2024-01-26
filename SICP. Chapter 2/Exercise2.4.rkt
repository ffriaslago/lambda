#lang racket

(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z
   (lambda (p q) p)))

(car (cons 5 6)) ; 5

; (car (cons x y)) -> (car (lambda (m) (m x y))) -> ((lambda (m) (m x y)) (lambda (p q) p))
; (lambda (p q) p) would be the parameter that works as the argument m in the other lambda function, leaving
; ((lambda (p q) p) x y)
; Now, x and y serve as the parameters that work as arguments p and q in the lambda function, leaving
; x

(define (cdr z) 
  (z
   (lambda (p q) q)))

(cdr (cons 5 6)) ; 6

; (cdr (cons x y)) -> (cdr (lambda (m) (m x y))) -> ((lambda (m) (m x y)) (lambda (p q) q))
; (lambda (p q) q) would be the parameter that works as the argument m in the other lambda function, leaving
; ((lambda (p q) q) x y)
; Now, x and y serve as the parameters that work as arguments p and q in the lambda function, leaving
; y


