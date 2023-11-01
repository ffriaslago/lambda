#lang racket

(define (make-interval a b) (cons a b))

; Following Alyssa's definitions, one could argue that she expects a to the be lower-bound and b the upper-bound
; This definitions work under that assumption

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define interval1 (make-interval 0 5))

(upper-bound interval1) ; 5
(lower-bound interval1) ; 0