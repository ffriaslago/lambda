#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) ; From the definition of map, (cons (proc (car items)) (map proc (cdr items)))
              empty
              sequence))

(map (lambda (x) (* x x)) (list 1 2 3 4)) ; '(1 4 9 16). Fine!

(define (append seq1 seq2)
  (accumulate cons
              seq2 ; if null? seq1->seq2
              seq1)) ; (cons (car sq1) (accumulate cons seq2 (cdr seq1))

(append (list 1 2 3) (list 4 5 6))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) ; Lambda has two arguments because of the definition of accumulate, op has two arguments.
              ; (+ 1 (accumulate (lambda (x y) (+ 1 y)) 0 (cdr sequence))). It is added 1 for each element
              0
              sequence)) 
