#lang sicp

; Auxiliary lines

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; The original procedures are:

(define (lookup-variable-value var env) ; Arguments: the variable it has to look for and the environment
  
  (define (env-loop env)
    
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env))) ; Scan the list of variables in the first frame
          (scan (frame-variables frame)
                (frame-values frame)))))
  
  (env-loop env))

(define (set-variable-value! var val env) ; Arguments: the variable, the new value and the environment. 
  
  (define (env-loop env)
    
    (define (scan vars vals) 
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val)) ; Difference with respect to the scan in the lookup-variable procedure
            (else (scan (cdr vars) (cdr vals)))))
    
    (if (eq? env the-empty-environment) ; This block of code is the same as the one in lookup-variable 
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  
  (env-loop env))

(define (define-variable! var val env) ; Arguments: the name of the new variable, the value and the environment. 
  
  (let ((frame (first-frame env)))
    
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame)) ; Difference with respect to the scan in the other two procedures
            ((eq? var (car vars))
             (set-car! vals val)) ; Same as set-variable-value!
            (else (scan (cdr vars) (cdr vals)))))
    
    (scan (frame-variables frame)
          (frame-values frame))))

; Seeing the three of them, it can be concluded that all three have a scan procedure which performs different procedures depending on the case

(define (tranverse-extension var env nullvar_procedure eqvar_procedure emptyenv_procedure)
  
  (define (env-loop env)
       
    (define (scan vars vals) 
      (cond ((null? vars) (nullvar_procedure env)) 
            ((eq? var) (eqvar_procedure vals)) 
            (else (scan (cdr vars) (cdr vals)))))
       
    (if (eq? env the-empty-environment) 
        (emptyenv_procedure)
        (let ((frame (first-frame))) 
          (scan (frame-variables frame) 
                (frame-values frame)))))
  
  (env-loop env))

; Now the three procedures have to be redefined

(define (lookup-variable-value_v2 var env) 
  (tranverse-extension var 
                       env
                       (lambda (env) (lookup-variable-value_v2 var (enclosing-environment env))) 
                       (lambda (vals) (car vals))                       
                       (lambda () (error "Unbound variable" var)))) 
  
(define (set-variable-value_v2! var val env) 
  (tranverse-extension var 
                       env
                       (lambda (env) (set-variable-value_v2! var val (enclosing-environment env))) 
                       (lambda (vals) (set-car! vals val))                        
                       (lambda () (error "Unbound variable" var)))) 
  
(define (define-variable_v2! var val env) 
  (tranverse-extension var 
                       env
                       (lambda (env) (add-binding-to-frame! var val (first-frame env))) 
                       (lambda (vals) (set-car! vals val))                        
                       (lambda () (error "Empty environment")))) 
