#lang racket

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 
         1)
        ((even? n) 
         (square (fast-expt b (/ n 2))))
        (else 
         (* b (fast-expt b (- n 1))))))

(define (square x) (* x x))

; It is tried to recreated fast-prime with this definition of expmod

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

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) (current-inexact-milliseconds))

; Lines used to solve the exercise (times in microseconds)

(timed-prime-test 1009) ; Time: 3,966.31;
(timed-prime-test 1013) ; Time: 5,431.88.
(timed-prime-test 1019) ; Time: 3,971.44.

(timed-prime-test 10007) ; Time: 233,575.93.
(timed-prime-test 10009) ; Time: 220,170.41.
(timed-prime-test 10037) ; Time: 238,997.80.

; Apparently, it works, although it is much slower, around 25 times as long as previous definition for 1,000 primes and 1327 times for 10,000 primes.

; The problem is that it computes a very large number and then calculates its remainder. The other way, no such high numbers are used.

; Let's see for 1009

;(timed-prime-test 1009)
;(start-prime-test 1009 (runtime))
;(fast-prime? 1009 100)
;(fermat-test 1009)
;(try-it (+ 1 (random (- 1009 1))))
;(expmod a 1009 1009) being a between 1 and 1008, it can even compute 1008 to the power of 1009, an absurdly large number, it can take a lot of memory

(timed-prime-test 1000037) ; It takes a lot of time and memory, it is taking a while (more than 1 min), I might stop it.
; It took 342,075,884.03 microseconds = 342,08 s (more than 5 minutes)
; It didn't run out of memory, but it is a possibility, 1,000,037 is no a very large number