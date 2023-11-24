#lang sicp

; There are some definitions pending
; --> make wire
; --> get-signal
; --> set-signal!
; --> add-action!
; --> after-delay

; From the book
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

; Mine
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (not (or (= s1 1) (= s1 0))) (not (or (= s2 1) (= s2 0)))) (error "Invalid signal"))
        (else 0)))

; From the book
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

; From the book
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; Mine
(define (or-gate a1 a2 o)
  (let ((i1 (make-wire))
        (i2 (make-wire))
        (i3 (make-wire)))
    (inverter a1 i1)
    (inverter a2 i2)
    (and-gate i1 i2 i3)
    (inverter i3 o)
    'ok))

; The delay of this definition of the or-gate is 3 times the delay of inverter-delay plus the delay of and-delay
