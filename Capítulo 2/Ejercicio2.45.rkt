#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;(define (right-split painter n)  
;  (if (= n 0)
;      painter      
;      (let ((smaller (right-split painter (- n 1))))        
;        (beside painter (below smaller smaller)))))


;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))

; It is going to be used the same logic as in the subsection 1.3.4 with the derivate procedure

; Going back, it was defined the procedure deriv
;(define (deriv g)
;  (define dx 0.00001)
;  (lambda (x)
;    (/ (- (g (+ x dx)) (g x))
;       dx)))
; With this, we could define the function 3x^2 as the derivate of x^3
;(define (cube x) (* x x x))
;(define dercube
;  (deriv cube))
; And we could express f'(5) being f(x)=x^3 as
;(dercube 5) ; 75.00014999664018
; Because (deriv g) returns a procedure, in particular a lambda function, with one argument, a number

; split needs to be defined as a procedure that returns other procedure because we want to express as dercube was expressed as a form of deriv

;(define up-split (split below beside))
;(define right-split (split beside below))

; As seen above, original right-split or up-split took 2 arguments, so (split op1 op2) needs to be defined as a lambda function of two arguments: the painter and a number
; Once seen this, the code inside is done straighforward

(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op1 op2) painter (- n 1))))
          (op1 painter (op2 smaller smaller))))))

; (split op1 op2) is a procedure that takes two procedures as arguments and returns a procedure as value, the lambda function that takes a painter and a number n
; So, if up-split is (split below beside) it is needed to indicate over which elements is the lambda function going to operate

(define up-split
  (split below beside))

(define right-split
  (split beside below))

(paint (right-split einstein 4))

(paint (up-split einstein 1))