#lang racket

(define (make-from-mag-ang r a)
  (define (dispatch op)  
    (cond ((eq? op 'magnitude) (* r (cos a)))
          ((eq? op 'angle) (* r (sin a)))
          ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)