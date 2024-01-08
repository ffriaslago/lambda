#lang sicp

;
; Section A
;

(define (lookup-variable-value var env)
  
  (define (env-loop env)
    
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) *unassigned*) ; change made. It incorporates an if to check is the value found is the unassigned one
                 (error "The value found is unassigned" var) ; If yes, error message
                 (car vals))) ; If no, the same as before
            (else (scan (cdr vars) (cdr vals)))))
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  
  (env-loop env))
