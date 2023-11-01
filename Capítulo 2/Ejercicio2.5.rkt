#lang racket

(define (expt b n)
  (if (= n 0) 
      1 
      (* b (expt b (- n 1)))))

(define (cons x y) 
  (* (expt 2 x)
     (expt 3 y)))

(define (even? n)
  (= (remainder n 2) 0))

(define (car z) 
  (if (even? z)
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z) 
  (if (even? z)
      (cdr (/ z 2))
      (if (= 1 z)
          0
          (+ 1 (cdr (/ z 3))))))

(cdr (cons 7 4)) ; 4

(cdr (cons 7 0)) ; 0

(car (cons 40 0)) ; 40

