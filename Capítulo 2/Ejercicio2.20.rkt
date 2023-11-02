#lang racket

; First, let's define an auxiliary procedure to extract the elements with same parity from a list

(define (parity-filter parity l) ; parity is expected to be even? or odd?
  (if (null? l)
      empty
      (if (parity (car l))
          (append (list (car l)) (parity-filter parity (cdr l)))
          (append empty (parity-filter parity (cdr l))))))

(define (same-parity x . y)
  (if (null? y)
      x
      (if (even? x)
          (append (list x) (parity-filter even? y))
          (append (list x) (parity-filter odd? y)))))

(same-parity 1 2 3 4 5 6 7) ; '(1 3 5 7)

(same-parity 2 3 4 5 6 7) ; '(2 4 6)
