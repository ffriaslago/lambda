#lang racket

(define x (list (list 1 2) (list 3 4)))

(define (fringe l)
  (if (null? l)
        empty
        (if (pair? (car l))
            (append (fringe (car l)) (fringe (cdr l)))
            (append (list (car l)) (fringe (cdr l))))))

(fringe x)

(fringe (list x x))




            
  