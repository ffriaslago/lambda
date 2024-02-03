#lang racket

; New procedures in the problem formulation

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (display " no prime")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) (current-inexact-milliseconds))

; Old procedures needed for this exercise

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Lines used to solve the exercise

;(timed-prime-test 14)

; search-for-primes procedure

(define (search-for-primes r1 r2) ; r1 is the low bound and r2 the upper bound
  (if (even? r1)
      (search-for-primes (+ r1 1) r2) ; if r1 is even, it is skipped
      (cond ((> r1 r2) (newline) (display "Whole range checked")) ; if r1 is odd, first it is checked that the end of the range has not been reached yet
            (else (timed-prime-test r1) ; the prime test is performed over r1. It will display new information.
                  (search-for-primes (+ 2 r1) r2))))) ; To skip even integers, same procedure adding 2 to the low bound

; Three smallest primes larger than 1,000 (time in miliseconds)

;(search-for-primes 1001 1050)

;1009 *** 0.003173828125
;1013 *** 0.00244140625
;1019 *** 0.00244140625

; Average time -> 2.68 microseconds

; Three smallest primes larger than 10,000 (time in miliseconds)

;(search-for-primes 10001 10050)

;10007 *** 0.0048828125
;10009 *** 0.005615234375
;10037 *** 0.0068359375

; Average time -> 5.78 microseconds (ratio of 2.16 with respect to 1,000 primes times, sqrt(10)=3.16)

; Three smallest primes larger than 100,000 (time in miliseconds)

;(search-for-primes 100001 100050)

;100003 *** 0.01611328125
;100019 *** 0.01904296875
;100043 *** 0.015869140625

; Average time -> 17.01 microseconds (ratios of 6.35 and 2.94 with respect to 1,000 and 10,000 primes times respectively)

; Three smallest primes larger than 1,000,000 (time in miliseconds)

(search-for-primes 1000001 1000050)

;1000003 *** 0.047119140625
;1000033 *** 0.04833984375
;1000037 *** 0.045166015625

; Average time -> 46.87 microseconds (ratios of 17.49, 8.11 and 2.76 with resepect to 1,000, 10,000 and 100,000 primes times respectively)

; In conclusion, the ratio between the times is expected to be higher, it could be explore to do the same exercise looking for more primes.
; Anyway, I would say the results are compatible to the notion mentioned in the statment. 


