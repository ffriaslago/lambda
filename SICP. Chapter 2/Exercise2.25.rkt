#lang racket

;'(1 3 (5 7) 9)
(define l1 '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr l1))))) ; 7

;'((7))
(define l2 '((7)))
(car (car l2)) ; 7

;'(1 (2 (3 (4 (5 (6 7))))))
(define l3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))) ; 7

; If it is not implemented the first car, the outcome would be '(7) instead of the number 7