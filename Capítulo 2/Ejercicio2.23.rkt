#lang racket

(define (for-each proc items)
  (cond ((null? items) #t) ; It is used cond instead of if because two expressions are needed in the <alternative> clause
        (else (proc (car items)) (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88 99))