#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        (compose f identity)
        (compose f (repeated f (- n 1)))))

(define (expt b n)
  (if (= n 0) 
      1 
      (* b (expt b (- n 1)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

; First, let's do an experimentation to see the number of average damps it is needed

(define (n-root-damped x n times) ; x is the number, n the indicator of the n-th root, times indicates the number of dampings performed
  (fixed-point ((repeated average-damp times) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

; It is used the fixed-point procedure for the calculus of n-th roots, it needs one procedure and one first-guess (1.0)
; The procedure is ((repeated average-damp times) (lambda (y) (/ x (expt y (- n 1))))), where it is indicated how many times the average damping is performed in the procedure
; described by the lambda function, as the exponent of the function changes with n

; Let's x=2

; It's known

; n=2 -> times=1
; n=3 -> times=1
; n=4 -> times=2

; (n-root-damped 2 2 1) OK
; (n-root-damped 2 3 1) OK
; (n-root-damped 2 4 1) not OK
; (n-root-damped 2 4 2) OK
; ...
; (n-root-damped 2 7 2) OK
; (n-root-damped 2 8 2) not OK
; (n-root-damped 2 8 3) OK
; ...
; (n-root-damped 2 15 3) OK
; (n-root-damped 2 16 3) not OK
; (n-root-damped 2 16 4) OK
; ...
; (n-root-damped 2 31 4) OK
; (n-root-damped 2 32 4) not OK
; (n-root-damped 2 32 5) OK

;(n-root-damped 2 32 5)

; So the pattern follows

; n=2 -> 1 time
; n=4 -> change -> 2 times
; n=8 -> change -> 3 times
; n=16 -> change -> 4 times
; n=32 -> change -> 5 times

; (log x y) procedure in Lisp returns the logarithm of x base y
; (floor x) procedure in Lisp returns the integer part of x
; the number of times the damping is needed for the nth root of a number would be (floor (log n 2))

; The final procedure is an adaptation of the former one without the times argument, as now it is calculated inside the procedure depending on n

(define (n-root x n)
  (fixed-point ((repeated average-damp (floor (log n 2))) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(n-root 2 4) ; Result: 1.189207115002721