#lang racket

; f as a recursive process

(define (frec n)
  (if (< n 3)
      n
      (+ (frec (- n 1))
         (* 2 (frec (- n 2)))
         (* 3 (frec (- n 3))))))

(frec 3) ; 4
(frec 4) ; 11
(frec 5) ; 25
(frec 6) ; 59
(frec 7) ; 142

; f as an iterative process

(define (fiter n)
  (f-iter 0 1 2 n))

(define (f-iter a b c n)
  (if (= n 0)
      a
      (f-iter b
              c
              (+ c
                 (* 2 b)
                 (* 3 a))
              (- n 1))))

(fiter 3) ; 4
(fiter 4) ; 11
(fiter 5) ; 25
(fiter 6) ; 59
(fiter 7) ; 142
  