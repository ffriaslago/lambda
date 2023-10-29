#lang racket

; Recursive procedure. It was corrected after exercise 1.38 because it didn´t work with the Euler approximation because it was not built correctly
; It needs an auxiliary procedure named rec to keep the calculation rightly. As in the iterative way, it is calculated reversely

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

; Iterative procedure. It is started with the term k and then built backwards

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k)))) ; As a first argument, it is calculated the k-th fraction, so the first index is k-1
  
; n and d are the functions f(x)=1 for all x

; 1/phi=0.6180

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10) ; Result: 0.6179775280898876.

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) ; Result: 0.6180555555555556. 

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12) ; Result: 0.6180257510729613. 

