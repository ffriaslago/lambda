#lang racket

; intersection-set procedure for sets as ordered lists with growth O(n)

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(intersection-set '(1 2 3 4 5) '(3 4 5 6 7)) ; '(3 4 5)

; Attempt of union-set procedure for sets as ordered lists with growth O(n)

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '()) ; Both of them are null, empty set
        ((null? set1) set2) ; First one is empty, second one
        ((null? set2) set1) ; Second one is empty, first one
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1
                        (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) ; x1 is smaller than every element in set2
                  (cons x1
                        (union-set (cdr set1) set2)))
                 ((< x2 x1) ; x2 is smaller than every element in set1
                  (cons x2
                        (union-set set1 (cdr set2)))))))))

(union-set '(1 2 3 4 5) '(3 4 5 6 7)) ; '(1 2 3 4 5 6 7)


