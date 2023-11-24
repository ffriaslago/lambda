#lang sicp

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (make-wire)
  
  (let ((signal-value 0) 
        (action-procedures '()))
    
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)) ; When a new action procedure is added to a wire, the procedure is immediately run, as it has the proc command here

    ; Alternative version for the Exercise 3.31
    (define (accept-action-procedurev2! proc)
      (set! action-procedures (cons proc action-procedures)))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'add-actionv2!) accept-action-procedurev2!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; I'll go through the sample simulation of half adder to see why this is necessary and the difference

; Firstly, it creates an agenda and set the time delays for the different gates
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; Secondly, it creates the wires
(define input-1 (make-wire))
(define input-2 (make-wire)) ; initial-value 0
(define sum (make-wire)) ; initial-value 0
(define carry (make-wire)) ; initial-value 0

; Then, probe is placed in two wires, so 
(probe 'sum sum) ; -> (add-action! sum
;                                  (lambda() (newline) (display name) (display " ") (display (current-time the-agenda)) (display " New-value = ") (display (get-signal wire)))))
; The procedure runs and it returns
; sum-0 New-value = 0
(probe 'carry carry) ; -> (add-action! carry
;                                      (lambda() (newline) (display name) (display " ") (display (current-time the-agenda)) (display " New-value = ") (display (get-signal wire)))))
; The procedure runs and it returns
; carry 0 New-value = 0

(half-adder input-1 input-2 sum carry) ; ok
; (define d (make-wire))
; (define e (make-wire))

; (or-gate input-1 input-2 d)
; -> (add-action! input-1 or-action-procedure). The procedure or-action-procedure runs
; ---> (after-delay or-gate-delay (lambda () (set-signal! d (logical-or (get-signal input-1) (get-signal input-2)))
; -----> (add-to-agenda! (+ or-gate-delay (current-time the-agenda)) (lambda () (set-signal! d (logical-or (get-signal input-1) (get-signal input-2)) the-agenda)
; -> (add-action! input-2 or-action-procedure). The procedure or-action-procedure runs
; ... Same as above

; (and-gate input-1 input-2 carry)
; -> (add-action! input-1 and-action-procedure). The procedure and-action-procedure runs
; ---> (after-delay and-gate-delay (lambda () (set-signal! carry (logical-and (get-signal input-1) (get-signal input-2)))
; -----> (add-to-agenda! (+ and-gate-delay (current-time the-agenda)) (lambda () (set-signal! carry (logical-and (get-signal input-1) (get-signal input-2)) the-agenda)
; ->(add-action! input-2 and-action-procedure)
; ... Same as above

; (inverter carry e)
; -> (add-action! carry invert-input). The procedure invert-input runs.
; ...

; (and-gate d e sum)
; -> (add-action! d and-action-procedure) ...
; -> (add-action! e and-action-procedure) ...

; Now the agenda is updated with times and procedures and action-procedures is not empty
; If the procedure wasn't run with the (proc) instruction in the accept-action-procedure!, after-delay procedure wouldn't be called for each action and
; the agenda would be empty
; Now the agenda is updated and ordered.

; If accept-action-procedure! were as in the exercise instructions, action-procedures would be updated, but agenda would be empty, so
; if propagate is called, it would return 'done without doing anything, the values are not updated neither. 



