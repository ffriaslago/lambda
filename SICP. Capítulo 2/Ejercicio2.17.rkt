#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) 
                (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
      
(define l1 (list 23 72 149 34))

(list-ref l1 3) ; 34
(length l1) ; 4

; One easy definition would be to use two other procedures already defined in the book
(define (last-pair l)
  (list-ref l (- (length l) 1)))

(last-pair (list 23 72 149 34)) ; 34. It appears to work, but it returns the value, not the item itself, so this procedure is not what was looked for

; Let's try to do it without the auxiliary procedures
(define (last-pairv2 l)
  (let ((f (cdr l))) ; f is every item in the list but the first one
  (if (null? f) ; if f is null, the list is made of one item, so it gives back the first one
      l
      (last-pairv2 f)))) ; if not, it is performed the same procedure over the list without the first element, moving one element forward each time until the last one

(last-pairv2 (list 23 72 149 34)) ; '(34). This returns the element! It works