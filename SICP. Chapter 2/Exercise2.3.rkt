#lang racket

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (make-point a b) (cons a b))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

; A rectangle can be defined in some different ways. This exercise will be done with a naive approach. Assuming the user knows how to built a rectangle.

; As it is asked to apply the same perimeter and area procedures for both representations, let's start with them, very simple

(define (rectangle-perimeter r)
  (* 2 (+ (rectangle-base r) (rectangle-height r))))

(define (rectangle-area r)
  (* (rectangle-base r) (rectangle-height r)))

; First representation: three points, assuming the right angle is formed by the segment p1p2 and p2p3, i.e., base would be p1p2 and height p2p2

(define (make-rectangle1 p1 p2 p3)
  (cons p1 (cons p2 p3)))

(define (square x) (* x x))

;(define (rectangle-base r)
;   ;p1 (car r)
;   ;p2 (car (cdr r))
;   ;length p1p2 = sqrt((p2x-p1x)^2+(p2y-p1y)^2)
;  (sqrt (+ (square (- (x-point (car (cdr r))) (x-point (car r))))
;           (square (- (y-point (car (cdr r))) (y-point (car r)))))))

;(define (rectangle-height r)
;  ; p2 (car (cdr r))
;  ; p3 (cdr (cdr r))
;  ; length p2p3 = sqrt((p3x-p2x)^2+(p3y-p2y)^2)
;  (sqrt (+ (square (- (x-point (cdr (cdr r))) (x-point (car (cdr r)))))
;           (square (- (y-point (cdr (cdr r))) (y-point (car (cdr r))))))))

; Second representation: two segments, assuming they have a common starting/ending point and that they form a right angle,i.e., s1 would be the base and s2 the height

(define (make-rectangle2 s1 s2)
  (cons s1 s2))

(define (rectangle-base r)
  ; s1 (car r)
  ; length s1 = sqrt((endxs1-startxs1)^2+(endys1-startys1)^2)
  (sqrt (+ (square (- (x-point (end-segment (car r))) (x-point (start-segment (car r)))))
           (square (- (y-point (end-segment (car r))) (y-point (start-segment (car r))))))))

(define (rectangle-height r)
  ; s2 (cdr r)
  ; length s2 = sqrt((endxs2-startxs2)^2+(endys2-startys2)^2)
  (sqrt (+ (square (- (x-point (end-segment (cdr r))) (x-point (start-segment (cdr r)))))
           (square (- (y-point (end-segment (cdr r))) (y-point (start-segment (cdr r))))))))

; Rectangle constructions

(define p1 (make-point 5 0))
(define p2 (make-point 0 0))
(define p3 (make-point 0 3))

;(define r1 (make-rectangle1 p1 p2 p3))
;
;(rectangle-perimeter r1) ; 16. It works!
;
;(rectangle-area r1) ; 15. It works!

(define s1 (make-segment p1 p2))
(define s2 (make-segment p2 p3))

(define r2 (make-rectangle2 s1 s2))

(rectangle-perimeter r2) ; 16. It works!

(rectangle-area r2) ; 15. It works!
  
  
  
  
