#lang racket

; Original procedure
;(define (reverse l)
;    (if (null? l)
;        empty
;        (append (reverse (cdr l)) (list (car l)))))

(define (fold-right op initial sequence) ; also accumulate
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op 
                      initial 
                      (cdr sequence)))))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) empty sequence))

(reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) empty sequence))

(reverse (list 1 4 9 16 25)) ;(25 16 9 4 1)