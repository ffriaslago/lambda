#lang racket

(define (pascal l p) ; l refers to the line and p to the position in the Pascal Triangle. Example: line 4 is 1 3 3 1, first position is a 1, second a 3, third a 3, fourth a 1.
  (if (or (= p 1) (= p l))
      1
      (+ (pascal (- l 1) p)
         (pascal (- l 1) (- p 1)))))

(pascal 5 3) ; 6 (Line 5 is 1 4 6 4 1, so the third position is a 6)
(pascal 5 2) ; 4
(pascal 5 4) ; 4