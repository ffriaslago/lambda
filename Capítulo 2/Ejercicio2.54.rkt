#lang racket

(define (equal? a b)
  (if (and (not (pair? a)) (not (pair? b))) ; if a is a symbol (instead of a list!!), (pair? a) will return #f. Same with (pair? b). So, if a and b are symbols, this will return #t
      (eq? a b)
      ; if not, they are lists, division in car and cdr
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))

(equal? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6)) ; t

(pair? '(1 2 3 (4 5) 6)) ; #t

(pair? '(1 2 3)) ; #t

