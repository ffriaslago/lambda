#lang racket

(define (make-monitored proc)
  (let ((counter 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls) counter)
            ((eq? m 'reset-count) (set! counter 0))
            (else (set! counter (add1 counter)) (proc m))))
  dispatch))

(define s (make-monitored sqrt))

(s 100) ; 10

(s 'how-many-calls) ; 1

(s 4) ; 2

(s 'how-many-calls) ; 2

(s 'reset-count)

(s 'how-many-calls) ; 0