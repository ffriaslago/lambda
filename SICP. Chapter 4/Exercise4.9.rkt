#lang sicp

; Let's try to replicate the loop for with an usual sintax:

;'(for (i 0) (< i 10) (+ i 1) ⟨body⟩)

; Using a named-let

;(let loop-for ((i 0))
;  (if (< i 10)
;      (begin
;        ⟨body⟩
;        (loop-for (+ i 1)))))

; (car exp) -> for*
; (cdr exp) -> ((i 0) (< i 10) (+ i 1) ⟨body⟩)
; (cadr exp) -> (i 0)
; (cddr exp) -> ((< i 10) (+ i 1) ⟨body⟩)
; (cdddr exp) -> ((+ i 1) ⟨body⟩)
; (cddddr exp) -> ⟨body⟩

(define (for? exp) (tagged-list? exp 'for))

(define (for-body exp) (car (cddddr exp))) ; ⟨body⟩
(define (for-iterator exp) (caadr exp)) ; i
(define (for-initialvalue exp) (cadadr exp)) ; i
(define (for-maxiter exp) (caddr (caddr exp))) ; 10
(define (for-predicate exp) (caddr exp)) ; (< i 10)
(define (for-step exp) (cadddr exp)) ; (+ i 1)

(define (for->named-let exp)
  (list 'let 'loop-for (list (list (for-var exp) (for-initialvalue exp)))
        (list 'if (for-predicate exp)
              (cons
               (cons 'begin (for-body exp))
               (list 'go (for-step exp))))))