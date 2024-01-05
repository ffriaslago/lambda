#lang racket

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; '(1 2 3 4 5 6). One list of 6 elements
(cons x y) ; '((1 2 3) 4 5 6). One list of 4 elements, being the first one a list of three elements.
(list x y) ; '((1 2 3) (4 5 6)). One list of 2 elements, being both of them a list of 3 elements