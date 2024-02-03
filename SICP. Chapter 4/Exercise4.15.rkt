#lang sicp

; two options: a procedure returns a value o it doesn't, giving an error message or running forever

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

(try try)

(if (halts? try try)
    (run-forever) ; if (halts? try try) is true, it means that (try try) returns a value, but because of the code, it triggers (run-forever) and it runs forever
    ; so it doesn't halt despite of (halts? try try) being true
    'halted) ; if (halts? try try) is false, it means that (try try) doesn't return a value, but because of the code, it triggers 'halted and it returns something

; either way, it is a contradiction