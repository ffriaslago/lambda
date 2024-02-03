#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (identity x) x)

(define (repeated f n)
    (if (= n 1)
        (compose f identity)
        (compose f (repeated f (- n 1)))))

((repeated square 2) 5) ; 625. OK!