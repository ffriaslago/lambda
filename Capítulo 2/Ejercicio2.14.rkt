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

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

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

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

; Let's make two intervals

(define int1 (make-center-percent 40 0.05))
(define int2 (make-center-percent 50 0.04))

; Let's check if we get something different

(center  (par1 int1 int2)) ; 22.40
(percent (par1 int1 int2)) ; 0.13

(center  (par2 int1 int2)) ; 22.22 It's slightly different
(percent (par2 int1 int2)) ; 0.05 It's clearly different

; Other arithmetic expressions

(center  (div-interval int1 int1)) ; 1.0050125313283207 (should be 1)
(percent (div-interval int1 int1)) ; 0.09975062344139651

(center  (div-interval int1 int2)) ; 0.8028846153846154 it's correct up to some decimals
(percent (div-interval int1 int2)) ; 0.08982035928143708 too

(define int3 (make-center-percent 40 0.025))
(define int4 (make-center-percent 50 0.001))

(center  (div-interval int3 int3)) ; 1.0012507817385867
(percent (div-interval int3 int3)) ; 0.049968769519050577

(center  (div-interval int3 int4)) ; 0.8000208000208
(percent (div-interval int3 int4)) ; 0.025999350016249596

; Dividing adds the percentages (as expected for small ones, see Exercise 2.13)


