#lang racket

(define (square x) (* x x))

;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (square (car things)) answer))))
;  (iter items empty))
;
;(square-list (list 1 2 3 4)) ; '(16 9 4 1)

; Let's dive into the procedure
; (square-list (list 1 2 3 4)) -> (iter (list 1 2 3 4) empty) -> (null? (list 1 2 3 4)) #f -> (iter (cdr (list 1 2 3 4)) (cons (square (car (list 1 2 3 4))) empty)) ->
; (iter (list 2 3 4) (cons 1 empty)) -> (iter (list 2 3 4) '(1)) -> (null? (list 2 3 4)) #f -> (iter (list 3 4) (cons 4 '(1))) -> (iter '(3 4) '(4 1))

; Seeing this, the answer is in reverse order because of the internal order of (cons (square (car things)) answer), as using there (car things), it stores the first element
; of the original list firstly, i.e, in the last place of the resulting list

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items empty))

(square-list (list 1 2 3 4)) ; '((((() . 1) . 4) . 9) . 16)

; Let's dive into the procedure
; (square-list (list 1 2 3 4)) -> (iter (list 1 2 3 4) empty) -> (null? (list 1 2 3 4)) #f -> (iter (cdr (list 1 2 3 4)) (cons empty (square (car (list 1 2 3 4))))) ->
; (iter (list 2 3 4) (cons empty 1)) -> (iter (list 2 3 4) '(() 1)) -> (null? (list 2 3 4)) #f -> (iter (list 3 4) (cons'(() 1)  4)) -> (iter '(3 4) ((() . 1) . 4))

; The command (cons 1 empty) gives '(1), but (cons empty 1) gives '(() 1), not the same