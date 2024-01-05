#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse l)
    (if (null? l)
        empty
        (append (reverse (cdr l)) (list (car l))))) ; It is used (list (car l)) because append needs two lists as arguments and (car l) returns the value of the first element

(reverse (list 1 4 9 16 25))

