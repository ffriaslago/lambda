#lang racket

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))
      (display " no prime")))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    ;(= (expmod1 a n n) a))
    (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

; Old version. Fermat test
(define (expmod1 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (square (expmod1 base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base (expmod1 base (- exp 1) m))
          m))))

; New version. Miller-Rabin test
(define (expmod2 base exp m)
  (cond ((= exp 0) 1)        
        ((even? exp)  (if (and (< 1 base) (> (- m 1) base) (= (remainder (square base) m) 1))
                         0
                         (remainder (square (expmod2 base (/ exp 2) m)) m) ))
        (else (remainder (* base (expmod2 base (- exp 1) m)) m))))

(define (square x) (* x x))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime) (current-inexact-milliseconds))

; Comprobations

(timed-prime-test 561) ; without changing it, 561 is seen as a prime. So expmod2 should be changed. With expmod2, 561 is not prime.
(timed-prime-test 1105) ; no prime with expmod2!
(timed-prime-test 1729) ; no prime with expmod2!
(timed-prime-test 2465) ; no prime with expmod2!
(timed-prime-test 2821) ; no prime with expmod2!
(timed-prime-test 6601) ; no prime with expmod2!

; Now it's tested with the primes founded in previous exercises

(timed-prime-test 1000003) ; Prime!
(timed-prime-test 1000033) ; Prime!
(timed-prime-test 1000037) ; Prime!