#lang racket

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var)))) ; Ejercicio2.73.rkt:11:16: get: unbound identifier in: get 
 
(define (operands exp) (cdr exp))

;
; Section A
;

; Instead of separating the different operations with the predicates sum? or product?, all of that has been replaced by a clause that gets the operation
; from a table using get, where <op> is 'deriv and <type> the operation wanted. It is used single dispatch on the type of the expression.
; With the selector operator it is obtained the operation and with operands it is obtained the content of it.
; number? and variable? are predicates for expressions that are not lists, they are either a number, with no operator o a single variable, so they don't have a tag

;
; Section B
;

; using (define (make-sum a1 a2) (list '+ a1 a2)) ; (define (make-product m1 m2) (list '* m1 m2)) and other procedures from the 2.3.2 section

; (deriv exp var) receives arguments such as ('(+ x y) 'x))
; and the arguments for the procedure from the table are (cdr '(+ x y)) and 'x, so the procedures for the derivatives have as arguments the operands of the expressions

(define (deriv-sum operands var)
  (make-sum (deriv (car operands) var) 
            (deriv (cdr operands) var)))

(put 'deriv '+ deriv-sum)

(define (deriv-product operands var)
  (make-product (deriv (car operands) var) 
                (deriv (cdr operands) var)))

(put 'deriv '* deriv-product)

;
; Section C
;

(define (deriv-expt operands var)
  (cond 
    ((= (cdr operands) 0) 0) 
    ((= (cdr operands) 1) (car operands)) 
    (else 
     (make-product 
      (cdr operands) 
      (make-exponent (car operands)
                     (- (cdr operands) 1))))))

(put 'deriv '** deriv-expt)

;
; Section D
;

(put '+ 'deriv deriv-sum)
(put '* 'deriv deriv-product)
(put '**  'deriv deriv-expt)

