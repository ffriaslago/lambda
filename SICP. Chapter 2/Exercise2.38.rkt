#lang racket

(define (fold-right op initial sequence) ; also accumulate
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(fold-right / 1 (list 1 2 3)) ; 1 1/2
;(/ 1 (fold-right / 1 '(2 3)))
;(/ 1 (/ 2 (fold-right / 1 '(3))))
;(/ 1 (/ 2 (/ 3 (fold-right / 1 '()))))
;(/ 1 (/ 2 (/ 3 1)))
;(/ 1 (/ 2 3))
;(/ 1 2/3)
;3/2

(fold-left  / 1 (list 1 2 3)) ; 1/6
;(iter 1 '(1 2 3))
;(iter (/ 1 1) '(2 3))
;(iter 1 '(2 3))
;(iter (/ 1 2) '(3))
;(iter 1/2 '(3))
;(iter (/ 1/2 3) '())
;(iter 1/6 '())
;1/6

(fold-right list empty (list 1 2 3)) ; '(1 (2 (3 ())))
;(list 1 (fold-right list empty '(2 3)))
;(list 1 (list 2 (fold-right list empty '(3))))
;(list 1 (list 2 (list 3 (fold-right list empty '()))))
;(list 1 (list 2 (list 3 empty)))
;(list 1 (list 2 '(3 ())))
;(list 1 '(2 (3 ())))
;'(1 (2 (3 ())))

(fold-left list empty (list 1 2 3)) ; '(((() 1) 2) 3)
;(iter empty '(1 2 3))
;(iter (list empty 1) '(2 3))
;(iter '(() 1) '(2 3))
;(iter (list '(() 1) 2) '(3))
;(iter '((() 1) 2) '(3))
;(iter (list ((() 1) 2) 3) '())
;(iter '(((() 1) 2) 3) '())
;'(((() 1) 2) 3)

; The property op should satisfy is to be conmutative
