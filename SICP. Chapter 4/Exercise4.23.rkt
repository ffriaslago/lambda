#lang sicp

; Book's version

(define (analyze-sequence exps)
  
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

; Sequence has just one expression

(procs) -> <exp1>

(loop (car procs) (cdr procs)) -> (car procs) -> <exp1>

; Sequence has two expressions

(procs) -> (<exp1> <exp2>)

(loop (car procs) (cdr procs)) -> (loop (sequentially (car procs) (cadr procs)) (cddr procs)) -> (sequentially (car procs) (cadr procs)) ->
(lambda (env) ((car procs) env) ((cdr procs) env))                                        

; Alyssa's version

(define (analyze-sequence exps)
  
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: 
                ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

; Sequence has just one expression

(procs) -> <exp1>

(lambda (env) (execute-sequence procs env)) -> (lambda (env) (car procs) env) -> (analyzed-exp1 env)

; Sequence has two expressions

(procs) -> (<exp1> <exp2>)

(lambda (env) (execute-sequence procs env)) -> (lambda (env) ((car procs) env)  (execute-sequence (cdr procs) env)) ; individual expression analyzed, but not the sequence itself 


