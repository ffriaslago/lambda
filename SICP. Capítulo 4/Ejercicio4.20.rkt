#lang sicp

;
; Section A
;

;(letrec ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
;  ⟨body⟩)
; car  -> letrec
; cadr -> (⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩)
; cddr -> ⟨body⟩

(define (letrec? expr)
  (tagged-list? expr 'letrec))

(define (letrec-inits expr)
  (cadr expr))

(define (letrec-vars exp)
  (map car (letrec-inits exp)))

(define (letrec-exps exp)
  (map cadr (letrec-inits exp))) 

(define (letrec-body expr)
  (cddr expr))

(define (declare-variables expr) 
  (map (lambda (x) (list (car x) '*unassigned*)) (letrec-inits expr)))

(define (set-variables expr) 
  (map (lambda (x) (list 'set! (car x) (cadr x))) (letrec-inits expr)))

; The idea is to express letrec as in this kind of let

;(let ((⟨var1⟩ '*unassigned*)
;      ...
;      (⟨varn⟩ '*unassigned*))
;  (set! ⟨var1⟩ ⟨exp1⟩)
;  (set! ⟨varn⟩ ⟨expn⟩))
;⟨body⟩

(define (letrec->let expr) 
  (list 'let (declare-variables expr)  
        (make-begin (append (set-variables expr) (letrec-body expr))))) ; (define (make-begin seq) (cons 'begin seq))

;
; Section B
;

;Original procedure

(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  ⟨rest of body of f⟩)

; Procedure with letrec

(define (f x)
  (letrec
      ((even?
        (lambda (n)
          (if (= n 0)
              true
              (odd? (- n 1)))))
       (odd?
        (lambda (n)
          (if (= n 0)
              false
              (even? (- n 1))))))
    ⟨rest of body of f⟩))

; Procedure as Luois Reasoner would want

(define (f x)
  (let ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    "rest of body of f"))

; Substituting let by lambda functions

(define (f x)
  ((lambda (even? odd?) 
    "rest of body of f")    
   (lambda (n)
     (if (= n 0)
         true
         (odd? (- n 1))))
   (lambda (n)
     (if (= n 0)
         false
         (even? (- n 1))))))

; Here it lies the problem again, as even? and odd? would be unassigned in this lambda function