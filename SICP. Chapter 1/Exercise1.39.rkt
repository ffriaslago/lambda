#lang racket

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (tan-cf x k)
  (/ x (+ 1 (cont-frac (lambda (i) (- (* x x))) (lambda (i) (+ (* 2 i) 1)) k))))

(tan-cf 1 100) ; 1.5574077246549022305069748... Fine!

(tan-cf 2 100) ; -2.1850398632615189916433061... Fine!
