#lang racket

;(define (map proc items)
;  (if (null? items)
;      nil
;      (cons (proc (car items))
;            (map proc (cdr items)))))

; Footnote 78
;Scheme standardly provides a map procedure that is more general than the one described here. This more general map takes a procedure of n
;arguments, together with n lists, and applies the procedure to all the first elements of the lists, all the second elements of the lists,
;and so on, returning a list of the results. For example:
;
;(map + 
;     (list 1 2 3) 
;     (list 40 50 60) 
;     (list 700 800 900))
;(741 852 963)
;
;(map (lambda (x y) (+ x (* 2 y)))
;     (list 1 2 3)
;     (list 4 5 6))
;(9 12 15)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (dot-product v w) ; This definition uses the extended version of map described in Footnote 78.
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define mat1 '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define vec1 '(1 2 3 4))

(matrix-*-vector mat1 vec1) ; '(30 56 80). Great!

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose m)
  (accumulate-n cons empty m))

(transpose mat1) ; '((1 4 6) (2 5 7) (3 6 8) (4 6 9)). Great!

(define (matrix-*-matrix m n) ; Matrix m '((1 2 3 4) (4 5 6 6) (6 7 8 9))
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define mat2 '((1 2) (3 4) (5 6) (7 8)))

(matrix-*-matrix mat1 mat2) ; '((50 60) (91 112) (130 160)). Great!















