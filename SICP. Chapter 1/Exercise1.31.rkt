#lang racket

; Recursive procedure for product

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Iterative procedure for product

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; Factorial procedure

(define (identity x) x)

(define (inc n) (+ n 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 10) ; 3628800. It works perfectly

; Pi procedure

; Wallis product, in Wikipedia, describes the expression as the product from 1 to inf of (4n^2)/(4n^2-1)

(define (square x) (* x x))

(define (pi-term x) (/ (* 4 (square x)) (- (* 4 (square x)) 1))) 

(define (half-pi-sum n)
  (product pi-term 1 inc n))

(* 2 (half-pi-sum 1000)) ; 3.1408077460303945604858682... Fine!