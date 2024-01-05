#lang racket

; (let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)

(define (fib n)
  
  (let fib-iter ; ⟨var⟩. It is a procedure whose parameters are the variables in the ⟨bindings⟩. 
    
    ((a 1) (b 0) (count n)) ; ⟨bindings⟩. fib-iter has three parameters. 
    
    (if (= count 0)             ; ⟨body⟩ start
        b                       ; ⟨body⟩ 
        (fib-iter (+ a b)       ; ⟨body⟩                  
                  a             ; ⟨body⟩ 
                  (- count 1))) ; ⟨body⟩ end
    )
  )

; Auxiliary procedures as usual

(define (let? exp) (tagged-list? exp 'let))

(define (named-let? expr) (and (let? expr) (symbol? (cadr expr)))) 
  
(define (named-let-name expr) (cadr expr)) 
  
(define (named-let-body expr) (cadddr expr)) 
  
(define (named-let-variables expr) (map car (caddr expr))) 
  
(define (named-let-values expr) (map cadr (caddr expr)))

; This new one is to create the definition of the interior procedure inside named-let

(define (named-let-procedure exp)
  (list 'define
        (cons (named-let-name exp)
              (named-let-variables exp))
        (named-let-body)))            

; Then, the final task would result in

(define (named-let->combination exp)
  
  (if (named-let? exp)
      ; If true
      (sequence->exp
       (list (named-let-procedure exp) ; definition of the procedure 
             (cons (named-let-name) (named-let-values)))) ; procedure is called with the values of the bindings
      ; If false, old let->combination
      (cons (make-lambda (let-variables exp)
                         (let-body exp))
            (let-expressions exp))))
                         
      
