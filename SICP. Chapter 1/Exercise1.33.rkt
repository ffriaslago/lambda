#lang racket

; filtered-accumulate procedure (recursive)

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))

; a)

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

(define (inc n) (+ n 1))

; a.)

(filtered-accumulate + 0 square 1 inc 100 prime?) ; 65797. Fine! Checked with a calculator. Great!

; b) It is needed to write rel-prime?

(define (identity x) x)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (rel-prime? x)
  (= 1 (gcd x 20))) ; same b as in the filtered-accumulate call

(filtered-accumulate * 1 identity 1 inc 20 rel-prime?) ; 8729721=1·3·7·9·11·13·17·19. Great!


