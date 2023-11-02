#lang racket

(define (cc amount coin-values)
  (cond ((= amount 0) 1) ; Remembering Exercise 1.14, when amount was 0, it was added one to the total combinations
        ((or (< amount 0) (no-more? coin-values)) 0) ; If amount is negative or there are no more coins to check
        (else
         (+ (cc amount
             (except-first-denomination coin-values))
            (cc (- amount
                (first-denomination coin-values))
             coin-values)))))

(define (no-more? list)
  (null? list))

(define (except-first-denomination list)
  (cdr list))

(define (first-denomination list)
  (car list))

(define us-coins (list 1 25 10 5 50))

(cc 100 us-coins) ; 292. It works!

; The order of the coins does not affect the answer. It makes sense as it calcualtes all the possibilities independently of the value of the coin
