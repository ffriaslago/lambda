#lang racket

; Old procedures needed for this exercise

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next int)
  (if (= 2 int) 3 (+ 2 int)))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; timed-prime-test procedure

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

; Lines used to solve the exercise (times in microseconds)

(timed-prime-test 1009) ; Previous time: 3.17. Actual time: 3.41.
(timed-prime-test 1013) ; Previous time: 2.44. Actual time: 1.71.
(timed-prime-test 1019) ; Previous time: 2.44. Actual time: 1.71.

; -> Ratio: 2.68 vs 2.28 -> 1.18

(timed-prime-test 10007) ; Previous time: 4.88. Actual time: 4.88.
(timed-prime-test 10009) ; Previous time: 5.62. Actual time: 4.64.
(timed-prime-test 10037) ; Previous time: 6.84. Actual time: 4.64.

; -> Ratio: 5.78 vs 4.65 -> 1.24

(timed-prime-test 100003) ; Previous time: 16.11. Actual time: 11.96.
(timed-prime-test 100019) ; Previous time: 19.04. Actual time: 11.96.
(timed-prime-test 100043) ; Previous time: 15.87. Actual time: 11.47.

; -> Ratio: 17.01 vs 11.80 -> 1.44

(timed-prime-test 1000003) ; Previous time: 47.12. Actual time: 35.64.
(timed-prime-test 1000033) ; Previous time: 48.34. Actual time: 34.67.
(timed-prime-test 1000037) ; Previous time: 45.16. Actual time: 34.67.

; -> Ratio: 46.87 vs 34.99 -> 1.34

; The actual time is not half ot the previous time. It is lower, much observed for larger times, but bigger than expected.

; Possible explanation: Instead of performing a easy operation, adding 1 to a integer, now the procedure calls another procedure (next), which has an if with the condition (= 2 int).
; That has to take some time, it is not a direct skim. 


