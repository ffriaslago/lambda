#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      empty
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (unique-pairs n) ; Sequence of pairs (i,j) with 1<=j<i<=n
  (accumulate append
              empty
              ; arguments of map, a procedure and a list. The procedure is the lambda function and the list is (enumerate-interval 1 n)
              (map (lambda (i) 
                     (map (lambda (j) 
                            (list j i))
                          (enumerate-interval 1 (- i 1)))) ; For every i, enumerate the integers j<i and create a pair (list i j)
                   (enumerate-interval 1 n))))

(unique-pairs 5) ; '((1 2) (1 3) (2 3) (1 4) (2 4) (3 4) (1 5) (2 5) (3 5) (4 5)). Great!

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6) ; '((1 2 3) (2 3 5) (1 4 5) (3 4 7) (2 5 7) (1 6 7) (5 6 11)). Great!