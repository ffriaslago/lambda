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

(define (unique-triples n) ; Sequence of triples (i,j,k) with 1<=k<j<i<=n
  (accumulate append
              empty
              (map (lambda (i) ; Here, instead of concatenating lambda functions, it is inserted the unique-pair structure, like inserting i to a pair (k,j)
                     (accumulate append
                                 empty
                                 (map (lambda (j)
                                        (map (lambda (k)
                                               (list k j i))               
                                                 (enumerate-interval 1 (- j 1))))
                                      (enumerate-interval 1 (- i 1)))))      
                     (enumerate-interval 1 n))))

(unique-triples 5) ; '((1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 5) (1 3 5) (2 3 5) (1 4 5) (2 4 5) (3 4 5))

(define (make-triple-sum triple)
  (+ (car triple) (cadr triple) (car (cdr (cdr triple)))))

(define (goal-sum-triples n s)
  (filter (lambda (triple) (= (make-triple-sum triple) s)) ; filter uses a predicate and list as arguments. The list is clear, the predicate is whether the sum of the triple gives s
          (unique-triples n)))


(goal-sum-triples 5 10) ; '((2 3 5) (1 4 5))

(goal-sum-triples 10 20) ; '((5 7 8) (5 6 9) (4 7 9) (3 8 9) (4 6 10) (3 7 10) (2 8 10) (1 9 10))

