#lang sicp

(<test> -> <recipient>)

; Remainder: assoc procedure. Arguments are a key and a list of records. Assoc returns the record that has the given key as its car
; (assoc 'b '((a 1) (b 2))) returns (b 2)

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

; Here, test is (assoc 'b '((a 1) (b 2))), which returns (b 2), which is a true value, so the procedure cadr in recipient is evaluated on the value of test

; It is evaluated (cadr (b 2)), which returns 2, as it is stablished in the statment

(define (additional-syntax-cond? exp)
  (if (pair? exp)
      (eq? (cadr exp) '=>)
      false))

(define (cond-test exp) (car exp))
(define (cond-recipient exp) (caddr exp))

(define (expand-clauses clauses) ; From the book, let's modify it. 
  (if (null? clauses)
      'false    
      (let ((first (car clauses))
            (rest (cdr clauses)))

        (if (additional-syntax-cond? first)               ; New
            (let ((test (cond-test first)))               ; New
              (make-if test                               ; New
                       (list (cond-recipient first) test) ; New
                       (expand-clauses rest)))            ; New

        
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp 
                     (cond-actions first))
                    (error "ELSE clause isn't 
                        last: COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp 
                          (cond-actions first))
                         (expand-clauses 
                          rest))))))