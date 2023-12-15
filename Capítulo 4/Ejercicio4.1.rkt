#lang sicp

; Arguments:
; -> exps: operands of the combination
; -> env: environment

; In the eval definition, this is used in the case of combinations
;((application? exp)
; (apply (eval (operator exp) env) 
;        (list-of-values (operands exp) env)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; If the arguments to cons are evaluated from left to right, then list-of-values will evaluate operands from left to right
; If the arguments to cons are evaluated from right to left, then list-of-values will evaluate operands from right to left

; We need to find a way to evaluate (eval (first-operand exps) env) first and (list-of-values (rest-operands exps) env) second

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env))) ; As the let is closed here, it has to be evaluated first
        (let ((second (list-of-values-left-to-right (rest-operands exps) env)))
      (cons first second)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((first (list-of-values-right-to-left (rest-operands exps) env))) ; As the let is closed here, it has to be evaluated first
        (let ((second (eval (first-operand exps) env)))
      (cons second first))))) ; This order does not matter, what it matters is which has been evaluated first
            