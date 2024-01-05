#lang racket

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y) 
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.00000035245) ; Real answer 0.0005936749952625595 Result 0.031253755705301936
(sqrt 0.0000025)     ; Real answer 0.0015811388300841896 Result 0.03127663609719173
(sqrt 0.003)         ; Real answer 0.0547722557505166113 Result 0.05815193427238369

;(sqrt 1234567891238000) ; does not finish computing

; Explanation
; Encoding of floating point numbers in computers
; --> Finite number of bits, a floating point number is an approximation of a real number, rounding issues. The number of floating numbers that can be represented in a
;computer is finite.
; The gap between the real value and the computed value increases from iteration to iteration

; Large numbers: the computation of the square root will never finish
; Despite of applying (improve guess x) the guess is not improved enough and the difference is larger than 0.001 and can't be reduced. Indeed, guess remain constant after
; a large number of steps and the difference is not reduced.
; The smalles possible difference between guess^2 and x is larger than 0.001. The difference between two consecutive floating points number of certain size is bigger than 0.001.
; Is the precision is increased, to 0.00001 for example, it will be worse

; Small numbers: we cant have an accurate answer if x is smaller than the precision (0.001)
; The test indicates that the guess is good enough, although it may be far from the solution


; Redefinition of good-enough as proposed in the formulation

(define (good-enough2? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.000001))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess (improve guess x))
          guess
          (sqrt-iter2 (improve guess x) x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

(sqrt2 0.00000035245) ; Real answer 0.0005936749952625595 Result 0.0005936749994893595
(sqrt2 0.0000025)     ; Real answer 0.0015811388300841896 Result 0.0015811388301019491
(sqrt2 0.003)         ; Real answer 0.0547722557505166113 Result 0.054772255750587126

(sqrt2 1234567891238000) ; It finishes. Real answer 35136418.304061670 Result 35136434.557135284

