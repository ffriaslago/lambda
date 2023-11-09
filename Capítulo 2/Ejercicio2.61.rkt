#lang racket

; element-of-set? procedure for sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) false) ; no element can be in the empty set
        ((= x (car set)) true) ; if it is equal to the first one, true, end
        ((< x (car set)) false) ; as set is ordered, the smallest element is the first one, if x is even smaller, it is not in the set
        (else (element-of-set? x (cdr set))))) ; if x > (car set), it is repeated the same over (cdr set)

; adjoin-set procedure constructed by analogy with element-of-set?

(define (adjoin-set x set)
  (cond ((null? set) (list x)) ; x is added to the empty set
        ((= x (car set)) set) ; it is already in the set, no need to add
        ((< x (car set)) (cons x set)) ; as set is ordered, if x is smaller, it is not in the set and should be added in the first position
        (else (adjoin-set x (cdr set))))) ; if x > (car set), it is repeated the same over (cdr set)

; The same explanation on the growth can be used here, it is still O(n), but about n/2
        