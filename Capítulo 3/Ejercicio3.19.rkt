#lang sicp

(define (cycle? x) 
  (let ((initial-list x))
    (define (iter y) 
      (cond ((eq? y initial-list) #t) 
            ((null? y) #f) 
            (else (iter (cdr y))))) 
    (if (null? x) 
        #f 
        (iter (cdr x)))))

(define (iter y) 
  (cond ((eq? y initial-list) #t) 
        ((null? y) #f) 
        (else (iter (cdr y))))) 

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

;(cycle? z)
(define initial-list z)
z ; #0=(a b c . #0#)
(cdr z) ; #0=(b c a . #0#)
(cddr z) ; #0=(c a b . #0#)
(cdddr z) ; #0=(a b c . #0#) eq to z

;(cycle? z)
;(iter (cdr z))
;(iter (cddr z))
;(iter (cdddr z)) -> #t

; I would say the procedure takes only a constant amount of space, as it is iterative.
      
