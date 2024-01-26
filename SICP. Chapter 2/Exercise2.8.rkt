#lang racket

; Original procedure for adding two intervals

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

; Interval abstraction

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

; New procedure for substracting intervals.
; As the dividing procedure is defined using the multiplication procedure, substracting procedure will use addition procedure
; For the division, it is multiplied the first one by the reciprocal of the second
; For the substraction, one possibility is to add the first one to the opposite of the second, being this (- upper-bound, -lower-bound)

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))