#lang racket

(define (map proc items)
  (if (null? items)
      empty
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

;(map car s) 
;(cons (car (car s))
;      (map car (cdr s)))
;(cons 1
;      (map car '((4 5 6) (7 8 9) (10 11 12))))
;(cons 1
;      (cons (car (4 5 6))
;            (map car '((7 8 9) (10 11 12)))))
;...
;(cons 1 4 7 10)
;'(1 4 7 10)

;(map cdr s)
;(cons (cdr (car s))
;      (map cdr (cdr s)))
;(cons '(2 3)
;      (map cdr '((4 5 6) (7 8 9) (10 11 12))))
;(cons '(2 3)
;      (cons (cdr (4 5 6))
;            (map cdr '((7 8 9) (10 11 12)))))
;...
;(cons '(2 3) '(4 5) '(8 9) '(11 12))    
      

(accumulate-n + 0 s) ; '(22 26 30). Fine!
