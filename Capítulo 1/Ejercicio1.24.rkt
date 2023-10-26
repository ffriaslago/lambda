#lang racket

; New procedures in the problem formulation

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))
      (display " no prime")))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))

(define (square x) (* x x))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) (current-inexact-milliseconds))

; Lines used to solve the exercise (times in microseconds)
; It was selected 100 times as a parameter for all the numbers

(timed-prime-test 1009) ; Time: 139.89.
(timed-prime-test 1013) ; Time: 149.66.
(timed-prime-test 1019) ; Time: 138.92.

; -> Average time: 173.18

(timed-prime-test 10007) ; Time: 196.78.
(timed-prime-test 10009) ; Time: 150.15.
(timed-prime-test 10037) ; Time: 167.72.

; -> Average time 171.55. Ratio with respect to 1,000 primes: 0.99.

(timed-prime-test 100003) ; Time: 169.68.
(timed-prime-test 100019) ; Time: 176.27.
(timed-prime-test 100043) ; Time: 181.40.

;  -> Average time 175.78. Ratio with respect to 1,000 primes: 1.02. Ratio with respect to 10,000 primes: 1.02.

(timed-prime-test 1000003) ; Time: 222.90.
(timed-prime-test 1000033) ; Time: 197.27.
(timed-prime-test 1000037) ; Time: 205.57.

;  -> Average time 208.58. Ratio with respect to 1,000 primes: 1.20. Ratio with respect to 10,000 primes: 1.22. Ratio with respect to 100,000 primes: 1.19.

; As Fermat test has O(log n) growth, testing 10,000 primes should take log(10,000)-log(1,000)=1, i.e., same order of magnitude. 
; The data does not bear this out, as the ratio founded is 1.2.
; My feeling is that time depedns on the number of times parameter in the procedure fast-prime?, not on the size of the prime, although it is true that it increases a little bit.