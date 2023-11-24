#lang sicp

; First in, first out order

(define input-1 (make-wire)) ; input-1 value: 0
(define input-2 (make-wire)) ; input-1 value: 0
(define output (make-wire))  ; ouput value: 0

(and-gate input-1 input-2 output)
; -> (add-action! input-1 and-action-procedure). The procedure and-action-procedure runs
; ---> (after-delay and-gate-delay (lambda () (set-signal! output (logical-and (get-signal input-1) (get-signal input-2)))
; -----> (add-to-agenda! (+ and-gate-delay (current-time the-agenda)) (lambda () (set-signal! output (logical-and (get-signal input-1) (get-signal input-2)) the-agenda)
; ->(add-action! input-2 and-action-procedure)
; ---> (after-delay and-gate-delay (lambda () (set-signal! output (logical-and (get-signal input-1) (get-signal input-2)))
; -----> (add-to-agenda! (+ and-gate-delay (current-time the-agenda)) (lambda () (set-signal! output (logical-and (get-signal input-1) (get-signal input-2)) the-agenda)

; the action in the after delay calls are
; (lambda () (set-signal! output (logical-and (get-signal input-1) (get-signal input-2))))
; This triggers set-signal!. As everything is 0, output-value does't change, it is done nothing, nothing is added to the agenda

(propagate) ; empty agenda

(set-signal! input-1 0) ; signal-value doesn't change, call-each is not run. Empty agenda
(set-signal! input-2 1) ; signal-value does change, call each is run, one and-action-procedure is in the agenda.
(propagate) ; The agenda is emptied after ejecuting the and-action-procedure  

(set-signal! input-1 1) ; one and-action-procedure is added to the agenda
(set-signal! input-2 0) ; a second and-action-procedure is added to the agenda
(propagate) ; The agenda is emptied ejecuting first the and-action-procedure associated to a1 and then the associated to a2.

; In the input1 action, first one called in First in, First out, both inputs have value 1, so the logical-and returns 1
; In the input2 action, second one called in First in, First out, input-1 has value 1 and input-2 has value 0, so the logical-and returns 0.
; Final output is 0, which is correct for and and-gate of different values.

; In the Last In, First Out.
; input2 action sets the output to 0
; input1 action sets the output to 1, as the value of input2 inside this is 1, then the final output would be 1, which is wrong.