#lang racket

; Old procedure

;(define rand
;  (let ((x random-init))
;    (lambda () (set! x (rand-update x)) x)))

; New procedure for the exercise.
; It is done under the assumption that rand-update exists, which it doesn't currently

(define rand 
  (let ((x random-init))
    (define (dispatch message)
      (cond ((eq? message 'generate) ; 'generate symbol 
             (set! x (rand-update x))
             x)
            ((eq? message 'reset) ; 'reset symbol
             (lambda (new-value) (set! x new-value))))) 
    dispatch))