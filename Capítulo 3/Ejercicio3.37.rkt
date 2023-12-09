#lang sicp

(define (celsius-fahrenheit-converter x) 
  (c+ (c* (c/ (cv 9) (cv 5)) 
          x) 
      (cv 32))) 

(define (c+ x y) 
  (let ((z (make-connector))) 
    (adder x y z) 
    z))

(define (c- x y) 
  (let ((z (make-connector))) 
    (adder z y x) ; z+y=x -> x-y=z 
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x) ; z*y=x -> x/y=z
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))
  