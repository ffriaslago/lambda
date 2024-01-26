#lang sicp

; From the book.

; Each frame of an environment is represented as a pair of lists: a list of the variables bound in that frame and a list of the associated values.

;(define (make-frame variables values)
;  (cons (variables values)))
;
;(define (frame-variables frame) (car frame))
;
;(define (frame-values frame) (cdr frame))

; Now, we can represent frame as a list of bindings, where each binding is a name-value pair

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (make-frame variables values)
  (list (map make-binding variables values)))

(make-frame '(x y z) '(1 2 3)) ; --> (((x . 1) (y . 2) (z . 3)))
(car (make-frame '(x y z) '(1 2 3))) ; --> ((x . 1) (y . 2) (z . 3))


  