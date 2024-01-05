#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (percent i)
  (/ (/ (- (upper-bound i) 
        (lower-bound i)) 
     2)
     (center i)))

; Example

(define int1 (make-center-percent 2 0.1)) ; (1.8,2.2)

(percent int1) ; 0.10000000000000003