#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))

(define (lower-bound interval) (car interval))

; Definition of width as expressed in the formulation of the problem

(define (width interval) (/ (- (upper-bound interval) (lower-bound interval)) 2))

; Adding intervals

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; width[(a,b)]=(a-b)/2
; width[(a,b)+(c,d)]=width[(a+c,b+d)]=(a+c-b-d)/2=(a-b)/2+(c-d)/2=width[(a,b)]+width[(c,d)]

; Example

(define int1 (make-interval 0 5))
(width int1) ; 2.5

(define int2 (make-interval 1 3))
(width int2) ; 1

(define int3 (add-interval int1 int2))
(lower-bound int3) ; 1
(upper-bound int3) ; 8

(width int3) ; 3.5 (2.5+1)

; Substracting intervals

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y)) (- (lower-bound y)))))

; width[(a,b)-(c,d)]=width[(a-d,b-c)]=(a-d-b+c)/2=(a-b)/2+(c-d)/2=width[(a,b)]+width[(c,d)]

; Example

(define int4 (sub-interval int1 int2))
(lower-bound int4) ; -3
(upper-bound int4) ; 4

(width int4) ; 3.5 (2.5+1)

; Multiplying intervals

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define int5 (mul-interval int1 int2))
(lower-bound int5) ; 0
(upper-bound int5) ; 15

(width int5) ; 7.5 (2.5,1)

(define int1.2 (make-interval -6 -1))
(width int1.2) ; 2.5

(define int5.2 (mul-interval int1.2 int2))
(lower-bound int5.2) ; -18
(upper-bound int5.2) ; -1

(width int5.2) ; 8.5 (2.5,1)

; The width of the inputs for int5 and int5.2 is the same, and the width of the outputs is not the same

; Dividing intervals

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define int6 (div-interval int1 int2))
(lower-bound int6) ; 0
(upper-bound int6) ; 5.0

(width int6) ; 2.5  (2.5,1)

(define int6.2 (div-interval int1.2 int2))
(lower-bound int6.2) ; -6
(upper-bound int6.2) ; -0.3333333

(width int6.2) ; 2.8333333 (2.5,1)

; The width of the inputs for int6 and int6.2 is the same, and the width of the outputs is not the same
