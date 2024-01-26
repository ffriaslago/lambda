#lang racket

; Recursive procedure combiner

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; Iterative procedure combiner

(define (accumulate combiner null-value term a next b)
  (define (iter-accumulate a result)
    (if (> a b)
        result
        (iter-accumulate (next a) (combiner (term a) result))))
    (iter-accumulate a null-value))

;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

; sum as accumulate
; combiner: +
; null-value: 0
; term, a, next, b: the same

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10) ; 3025. Fine!

;(define (product term a next b)
;  (if (> a b)
;      1
;      (* (term a)
;         (product term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

; product as accumulate
; combiner: *
; null-value: 1
; term, a, next, b: the same

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(factorial 10) ; 3628800. It works perfectly
