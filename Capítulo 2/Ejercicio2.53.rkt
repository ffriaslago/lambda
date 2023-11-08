#lang racket

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c) ; '(a b c)

(list (list 'george)) ; '((george))

(cdr '((x1 x2) (y1 y2))) ; '((y1 y2))

(cadr '((x1 x2) (y1 y2))) ; '(y1 y2)

(pair? (car '(a short list))) ; #f
(car '(a short list)) ; 'a
;(pair? 'a) returns #f
;(pair? a) returns error
;(pair? 2) returns #f


(memq 'red '((red shoes) (blue socks))) ; #f

(memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)
