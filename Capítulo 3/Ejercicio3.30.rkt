#lang sicp

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-list b-list s-list c) ; Assuming a-list, b-list and s-list are all of the same length and c is just one wire
  
  (if (or (null? a-list) (null? b-list))
      (error "A-list and B-list are empty" a-list b-list)
      (full-adder (car (a-list)) (car (b-list)) c (car (s-list)) c1)) ; It is computed the first full adder

  (if (or (null? (cadr (a-list)) (cadr (b-list))))
      ("ripple-carry-adder finished") ; The values of S_n and C_n are updated by the final call to full-adder
      (ripple-carry-adder (cdr (a-list)) (cdr (b-list)) c1 (cdr (s-list)) c-out))) ; If there are more elements, recursive call to compute more full-adders
  