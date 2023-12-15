#lang sicp

; Original eval code

(define (eval exp env)
  
  (cond ((self-evaluating? exp) exp)
        
        ((variable? exp) (lookup-variable-value exp env))
        
        ((quoted? exp) (text-of-quotation exp))
        
        ((assignment? exp) (eval-assignment exp env))
        
        ((definition? exp) (eval-definition exp env))
        
        ((if? exp) (eval-if exp env))
        
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        
        ((cond? exp) (eval (cond->if exp) env))
        
        ((application? exp) ; (define (application? exp) (pair? exp))
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        
        (else
         (error "Unknown expression type: EVAL" exp))))

; Why is the definition of application? just to check whether exp is a pair or not?
; As it is said in the book, it is defined this way because after checking all the other options in eval (remainder: cond works in a way that if some of the predicates is true
; it doesn't check the rest, it goes directly yo yhe consequent). This means that a procedure application is any compound expression that is not one of the above.

;
; Section A
;

; What Louis wants to do it to put all the application block before the assingment one. If the evaluator recieves (define x 3) this will happen

(pair? '(define x 3)) ; #t.

; Problem! The evaluator thinks (define x 3) is an application, not an assignment
; It will have operator as define
; operands as (x 3)
; It does not have any sense

; The problem with Louis idea is that every pair will be treated as an application, which is not what it is wanted

;
; Section B
;

; All the procedure applications will start with call. For example, instead of (+ 1 2), it will be (call + 1 2)

; We will change the procedure application? following similar examples in the book

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
; no-operands?, first-operand and rest-operands remain the same

