#lang racket

(let* ((x 3) ; assigns the value 3 to x
       (y (+ x 2)) ; the preceding binding is visible, so y is (+ 3 2), 5
       (z (+ x y 5))) ; Then z is (+ 3 5 5), 13
  (* x z)) ; (* 3 13), 39

;(define (let*? exp) (tagged-list? exp 'let*))

;(define (let*->nested-lets exp)

 ; )

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