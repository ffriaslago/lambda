#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '()) ; Both of them are null, empty set
        ((null? set1) set2) ; First one is empty, second one
        ((null? set2) set1) ; Second one is empty, first one
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2)) ; If the element is in both sets, it is not added (redundant)
        (else (cons (car set1) (union-set (cdr set1) set2))))) ; If the element is only in one set, it is added

(union-set '(1 2 3) '(3 5 6)) ; '(1 2 3 5 6). Great!