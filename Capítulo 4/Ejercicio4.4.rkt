#lang sicp

; and and or as special forms

(define (and? exp) (tagged-list? exp 'and)) ; The idea is that both and and or are identified with their name, as it was the case for if and con
(define (and-clauses exp) (cdr exp)) ; Then, it is identified all the expressions
(define (first-clause seq) (car seq))
(define (rest-clauses seq) (cdr seq))
(define (no-clauses? seq) (null? seq))
(define (last-clause? seq) (null? (cdr seq)))

(define (eval-and exps env) ; First it is build the evaluation procedure for and. The first argument, exps, refers to the and-clauses
  (cond ((no-clauses? exps) true) ; If there is not any clause, true by default      
        (else ; If there is at least one
         (let ((firstclause (eval (first-clause exps) env))) ; The first clause is evaluated.
           ; This is done because, as it is indicated, si all the expressions evaluate to true values, the value of the last expression is returned
           (cond ((last-clause? exps) firstclause) ; Then, if there is only one, the value of the last (and first) expression is returned
                 (first (eval-and (rest-clauses exps) env)) ; If first exists and it is true, the eval-and is continued
                 (else false)))))) ; otherwise, false is returned
             

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-or exps env)
  (cond ((no-clauses? exps) false)   
        (else 
         (let ((firstclause (eval (first-clause exps) env))) 
           (cond ((last-clause? exps) firstclause) ; Until here, it is the same, apart from changing the second line, from true to false
                 (first true) ; If first exists and it is true, there is no need to continue with the other expressions
                 (else (eval-or (rest-clauses exps) env))))))) ; otherwise, false is returned


; and and or as derived expressions

(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (if (no-clauses? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses))
            (if (last-clause? clauses)
                first
                (make-if first
                         (expand-and-clauses rest)
                         false))))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-and-clauses clauses)
  (if (no-clauses? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses))
            (if (last-clause? clauses)
                first
                (make-if first
                         true
                         (expand-or-clauses rest)))))))