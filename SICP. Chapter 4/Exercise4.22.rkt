#lang sicp

; Using the code from Exercise 3.6

(define (let? exp) (tagged-list? exp 'let))

(define (let-body exp)
  (cddr exp))

(define (let-varexps exp)
  (cadr exp))

(define (let-variables exp)
  (map car (let-varexps exp)))

(define (let-expressions exp)
  (map cadr (let-varexps exp)))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp)
                     (let-body exp))
        (let-expressions exp)))

(define (eval-let exp env)
  (eval (let->combination exp) env))

; It needs to be added the following line of code to the analyze procedure

((let? exp) (analyze (let->combination exp)))