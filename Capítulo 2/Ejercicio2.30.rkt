#lang racket

(define (square-tree tree)
  (cond ((null? tree) empty)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
 '(1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (* sub-tree sub-tree)
             (square-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
; '(1 (4 (9 16) 25) (36 49))        

