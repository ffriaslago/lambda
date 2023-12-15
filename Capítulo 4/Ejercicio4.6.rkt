#lang sicp

; Using

;(let ((⟨var₁⟩ ⟨exp₁⟩) … (⟨varₙ⟩ ⟨expₙ⟩))
;  ⟨body⟩)

; It can be assumed that

; (car exp) -> let
; (cdr exp) -> ((⟨var₁⟩ ⟨exp₁⟩) … (⟨varₙ⟩ ⟨expₙ⟩)) ⟨body⟩)
; (cadr exp) -> ((⟨var₁⟩ ⟨exp₁⟩) … (⟨varₙ⟩ ⟨expₙ⟩))
; (cddr exp) -> ⟨body⟩

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
