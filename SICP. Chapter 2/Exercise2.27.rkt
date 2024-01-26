#lang racket

; Reverse procedure from 2.18
(define (reverse l)
    (if (null? l)
        empty
        (append (reverse (cdr l)) (list (car l)))))

(define x (list (list 1 2) (list 3 4)))

x ; '((1 2) (3 4))

(reverse x) ; '((3 4) (1 2))

(define (deep-reverse l)
    (if (null? l)
        empty
        (append (deep-reverse (cdr l))
                (if (pair? (car l))
                    (list (reverse (car l)))
                    (list (car l))))))

(deep-reverse x) ; ((4 3) (2 1))