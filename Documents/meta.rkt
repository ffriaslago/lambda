#lang racket

; meta.rkt
; The metacircular evaluator from section 4.1

(define (seval exp environ)
  ; Evaluate a scheme expression
  (cond ((primitive? exp) exp)                            ; Primitive just "are". Return back
        ((symbol? exp) (lookup-environment exp environ))  ; Symbols? Look up in the environment.
        ((define? exp) (seval-define exp environ))
        ((if? exp) (seval-if exp environ))
        ((quote? exp) (seval-quote exp environ))
        ; ((cond? exp) ...)
        ; ((let ...))
        ; ((delay...))
        ((begin? exp) (seval-begin exp environ))
        ((lambda? exp) (seval-lambda exp environ))
        ((procedure-application? exp) (seval-procedure-application exp environ))
        (else (error "Unknown Expression"))
        )
  )

(define (primitive? exp)
  (or (number? exp) (boolean? exp)))

(define (procedure-application? exp)
  (list? exp)
  )

; (define name value)

; Predicate to test
(define (define? exp)
  (and (list? exp) (eq? (car exp) 'define))
  )

; Selectors to extract information from the expression
(define (define-name exp)
  (cadr exp)
)

(define (define-value exp)
  (caddr exp)
  )

; Evaluator. Do the thing. 
(define (seval-define exp environ)
  (let ((name (define-name exp))
        (value (define-value exp)))
    (define-in-environment! name (seval value environ) environ)
    )
  )

; (quote exp)

(define (quote? exp)
  (and (list? exp) (eq? (car exp) 'quote)))

(define (quote-expression exp)
  (cadr exp))

(define (seval-quote exp environ)
  ; Quoting doesn't do anything. It just returns the exp unevaluated.
  (quote-expression exp)
  )

; (if test consequence alternative)

(define (if? exp)
  (and (list? exp) (eq? (car exp) 'if)))

; Selectors
(define (if-test exp)
  (cadr exp)
  )

(define (if-consequence exp)
  (caddr exp)
  )

(define (if-alternative exp)
  (cadddr exp)
  )

(define (seval-if exp environ)
  (if (seval (if-test exp) environ)        ;  Evaluate the test first
      (seval (if-consequence exp) environ)
      (seval (if-alternative exp) environ)
      )
  )

; (begin exp1 ... expn)
; Evaluates all expressions, but only returns the result of the last one.

(define (begin? exp)
  (and (list? exp) (eq? (car exp) 'begin))
  )

(define (begin-expressions exp)
  (cdr exp)      ; Note: this returns a *list* of the expressions
  )

; Helper procedure to evaluate many expressions, returning the result of the last one
(define (seval-many exprs environ)
  (cond ((null? (cdr exprs))          ; Test if this is the last expression
         (seval (car exprs) environ))
        (else (seval (car exprs) environ) (seval-many (cdr exprs) environ))
        )
  )

(define (seval-begin exp environ)
  (seval-many (begin-expressions exp) environ)
  )

; (cond (test1 exp1 exp2 ... expn)
;        (test2 exp1 exp2 ... expn)
;        (test3 exp1 exp2 ..)
; Pulling apart cond is challenging.


; (lambda args exp1 ... expn)
;
; (lambda (x y) (+ x y))

(define (lambda? exp)
  (and (list? exp) (eq? (car exp) 'lambda)))

(define (lambda-args exp)
  (cadr exp))

(define (lambda-expressions exp)
  (cddr exp)
  )

(define (seval-lambda exp environ)
  ; What do you do here???
  ; Creates a "user procedure"  (procedure written in the interpreter we're making)
  (create-user-procedure (lambda-args exp) (lambda-expressions exp) environ)  ; <<< ENVIRONMENT MODEL!
  )

(define (create-user-procedure args exprs environ)
  ; Question: What do we make??? Will make a tagged list to indicate a user procedure.
  (list 'user-procedure args exprs environ)
  )

(define (user-procedure? proc)
  (and (list? proc) (eq? (car proc) 'user-procedure))
  )

; Selectors for getting parts of a user-defined procedure
(define (user-procedure-arguments proc)
  (cadr proc))

(define (user-procedure-expressions proc)
  (caddr proc))

(define (user-procedure-environment proc)
  (cadddr proc)
  )

; (proc arg1 arg2 ... argn)

(define (get-application-procedure exp)
  (car exp))

(define (get-application-arguments exp)
  (cdr exp)
  )

(define (seval-procedure-application exp environ)
  ; How does a procedure get evaluated?
  (let ((proc (seval (get-application-procedure exp) environ))    ; Must evaluate proc part to get the procedure
        (args (get-application-arguments exp)))
    (if (user-procedure? proc)
        (sapply-user-procedure proc args environ)
        (sapply-builtin-procedure proc args environ)
        )
    )
)

(define (sapply-user-procedure proc args environ)
  ; Evaluate a procedure created by my interpreter
  ; One reason for writing your own interpreter, is that you can change the rules of evaluation.
  ; (right-to-left, lazy evaluation, etc.). That would happen here.

  ; Checklist:
  (let (
  ;    0.  Must evaluate the passed arguments in the current environ
      (values (map (lambda (arg) (seval arg environ)) args))   ;  LAZY EVAL: Don't evaluate args!
  ;    1.  Must create a new environment  (for locals)
      (newenv (make-environment (user-procedure-environment proc)))
      )
  ;    2.  Bind the argument names to argument values (in the new environment)
    (bind-arguments (user-procedure-arguments proc) values newenv)
  ;    3.  Evaluate the expressions of the procedure body in the new environment
    (seval-many (user-procedure-expressions proc) newenv)
    )
  )

; This needs to bind the names of procedure arguments to procedure values in the
; specified environment.  argnames/values are both lists of the same size

(define (bind-arguments argnames values environ)
  (cond ((null? argnames) #t)
        (else (define-in-environment! (car argnames) (car values) environ)
              (bind-arguments (cdr argnames) (cdr values) environ)
              )
        )
  )

(define (sapply-builtin-procedure proc args environ)
  ; Apply a procedure to some arguments
  ; apply is part of Scheme, not something that I'm making up.
  (apply proc (map (lambda (arg) (seval arg environ)) args))
  ;            ^^^^ evaluate the procedure arguments
  )
  

; ----------------- Environments ------------
; Hints: You *CAN* use a Racket hash table if you want.
; Think about nested-environments perhaps.   Also, think about the existence of a global
; environment.  And definitions that might go into it.

; The environment is actually a list of hash tables.  You need to provide a parent environment
(define (make-environment parent)
  (cons (make-hash) parent)
  )

(define (lookup-environment name environ)
  (cond ((null? environ) (error "Name not found!"))
        ((hash-has-key? (car environ) name) (hash-ref (car environ) name))
        (else (lookup-environment name (cdr environ))))
  )

(define (define-in-environment! name value environ)
  (hash-set! (car environ) name value)
  )





; Define the "global" environment
(define environ (make-environment null))

; Put definitions in the environment
(define-in-environment! 'foo 123 environ)

; Built-in operators.  These are operations that will be provided, but are actually
; part of the core Racket environment used to implement the interpreter.
(define-in-environment! '+ + environ)
(define-in-environment! '- - environ)
(define-in-environment! '* * environ)
(define-in-environment! '/ / environ)
(define-in-environment! '< < environ)
(define-in-environment! '<= <= environ)
(define-in-environment! '> > environ)
(define-in-environment! '>= >= environ)
(define-in-environment! '= = environ)

; Define pairs (exercise 2.4)
(seval '(define cons (lambda (x y) (lambda (m) (m x y)))) environ)
(seval '(define car (lambda (p) (p (lambda (a b) a)))) environ)
(seval '(define cdr (lambda (p) (p (lambda (a b) b)))) environ)


; How to test

(define (check-equal? v expected fail)
  (if (equal? v expected)
      #t
      (displayln fail)
      )
  )

(check-equal? (seval '42 environ) 42 "Primitives failed")
(check-equal? (seval 'foo environ) 123 "Symbol lookup failed")
(seval '(define x 42) environ)
(check-equal? (seval 'x environ) 42 "Simple define failed")
(seval '(define y (+ 2 3)) environ)
(check-equal? (seval 'y environ) 5 "Expression define failed")
(check-equal? (seval '(quote x) environ) 'x "Quoting failed")

(check-equal? (seval '(if (< 2 3) 1 (/ 1 0)) environ) 1 "if-true failed")
(check-equal? (seval '(if (< 3 2) (/ 1 0) 1) environ) 1 "if-false failed")

; Procedures
(seval '(define square (lambda (x) (* x x))) environ)
(check-equal? (seval '(square 4) environ) 16 "square failed")

(seval '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) environ)
(check-equal? (seval '(fact 5) environ) 120 "fact failed")