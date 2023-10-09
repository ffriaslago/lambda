#lang racket

(define (sum-sq x y z)
  (cond ((and (not (< x z)) (not (< y z))) (+ (* x x) (* y y)))
        ((and (not (< x y)) (not (< z y))) (+ (* x x) (* z z)))
        ((and (not (< y x)) (not (< z x))) (+ (* y y) (* z z)))))

(sum-sq 1 2 3) ; 13
(sum-sq 3 1 2) ; 13
(sum-sq 2 3 1) ; 13