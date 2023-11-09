#lang racket

; Allowing duplications
; Set {1, 2, 3} represented with the list (2 3 2 1 3 2 2)

; Element-of-set?

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; It doesn't need to change, once it is founded one instance, it is not needed to check the rest
; Same growth O(n)

(element-of-set? 2 '(2 3 2 1 3 2 2)) ; #t

; Adjoin-set

(define (adjoin-set x set)
  (cons x set))

; As duplications are allowed, it can be added an element which is already in the set
; Growth O(1) it doesn't depend on n

(adjoin-set 3 '(2 3 2 1 3 2 2)) ; '(3 2 3 2 1 3 2 2)

; Union-set

(define (union-set set1 set2)
  (append set1 set2))

; As duplications are allowed, every element of both sets will be in the final set
; Growth O(1), it doesn't depend on n

(union-set '(2 4 4 5) '(2 3 2 1 3 2 2)) ; '(2 4 4 5 2 3 2 1 3 2 2)

; Intersection-set

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; It doesn't need to change
; Same growth O(n^2)

(intersection-set '(4 4 5) '(2 3 2 1 3 2 2)) ; '()
(intersection-set '(1 2 2 4) '(2 3 2 1 3 2 2)) ; '(1 2 2). It duplicates 2, but as it is a valid representation, it's ok

; Applications: collections of objetcs where you are able to have more than one instance of the same thing, like historic coins, in order to exchange them
; for coins you don't have