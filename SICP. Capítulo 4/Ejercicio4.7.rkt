#lang racket

(let* ((x 3) ; assigns the value 3 to x
       (y (+ x 2)) ; the preceding binding is visible, so y is (+ 3 2), 5
       (z (+ x y 5))) ; Then z is (+ 3 5 5), 13
  (* x z)) ; (* 3 13), 39

;
; Extra Exercise: Expressing let* with lambda functions
;

; Using

;(let ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
;  ⟨body⟩)

; It can be assumed that

; (car exp) -> let
; (cdr exp) -> ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩)) ⟨body⟩)
; (cadr exp) -> ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
; (cddr exp) -> ⟨body⟩


;(define (let? exp) (tagged-list? exp 'let))

(define (let-body exp)
  (cddr exp))

(define (let-varexps exp)
  (cadr exp))

(define (let-variables exp)
  (map car (let-varexps exp)))

(define (let-expressions exp)
  (map cadr (let-varexps exp)))

;(define x 4)
;(define y 7)
;(define z 11)

(define (let*->combination exp) ; usar de forma recursiva el let.
  (if (= (length (cadr exp)) 1)
      (cons (make-lambda (let-variables exp)
                         (let-body exp))
            (let-expressions exp))
      (list (make-lambda (list (car (let-variables exp)))
                         (list (let*->combination (list 'let* (cdadr exp) (car (let-body exp))))))
            (car (let-expressions exp)))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(eval '(let*->combination '(let* ((x 4) (y (+ x 3)) (z (+ x y))) (+ x y z))))

;((lambda (x) ((lambda (y) ((lambda (z) (+ x y z)) (+ x y))) (+ x 3))) 4)

;((lambda (z) (+ x y z)) (+ x y))

;((lambda (y) ((lambda (z) (+ x y z)) (+ x y))) (+ x 3))

;((lambda (x) ((lambda (y) ((lambda (z) (+ x y z)) (+ x y))) (+ x 3))) 4)
;((lambda (x) ((lambda (y) ((lambda (z) (+ x y z)) (+ x y))) (+ x 3))) 4)

;(let ((x 4)
;      (y (+ x 3)))
;  (+ x y))
;                  
;((lambda (x)
;   
;   ((lambda (y)
;      (+ x y))
;    (+ x 3)))
; 
; 4)

;
; Original Exercise
;

(define (let*? exp) (tagged-list? exp 'let*))

; '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; (car exp) -> let*
; (cdr exp) -> (((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
; (cadr exp) -> ((x 3) (y (+ x 2)) (z (+ x y 5)))
; (cddr exp) -> ((* x z))
; (caddr exp) -> (* x z)

(define (let*-body exp)
  (caddr exp))

(define (let*-varexps exp)
  (cadr exp))

(define (make-let varexps body)
  (cons 'let (cons varexps body)))

; Construction of let*->nested-lets using the procedure from the book sequence->exp, that transforms a sequence into a single expression

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (let*->nested-lets exp) ; The idea is to create the lets manually
  (define (iter-let varexps body)
    (if (null? varexps)
        (sequence->exp body)
        (make-let (list (car args))
                  (list (iter-let (cdr varexps) body)))))
  (iter-let (let*-varexps exp) (let*-body exp)))

; Question, is it sufficient to add a clause to eval whose action is
;
; (eval (let*->nested-lets exp) env)
;
; or must we explicitly expand let* in terms of non-derived expressions

; As the question is under the assumption that exercise 3.6 is implemented, eval can handle let and with let*->nested-lets, let* is handled as let expressions, so
; with the expression (eval (let*->nested-lets exp) env) is enough

        
    






