#lang racket

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)

; (lambda (f) (lambda (x) (f ((zero f) x))))

; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

; With (((lambda (f) (lambda (x) x)) f) x) -> ((lambda (x) x) x) -> x

; Leaving (lambda (f) (lambda (x) (f x)))

; (add-1 zero)=(lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

; (add-1 one)

; (lambda (f) (lambda (x) (f ((one f) x))))

; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))

; With (((lambda (f) (lambda (x) (f x))) f) x) -> ((lambda (x) (f x)) x) -> (f x)

; (add-1 one)=(lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

; From here

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(define (addition a b) ; assuming the inputs are the Church numerals
  ; What is wanted is to apply (f ...(f x)) as many times as a+b
  ; Seeing add-1, ((n f) x) gives one extra application of f
  ; ((zero f) x)->(((lambda (f) (lambda (x) x)) f) x) -> ((lambda (x) x) x) -> x
  ; ((one f) x)->(((lambda (f) (lambda (x) (f x))) f) x) -> ((lambda (x) (f x)) x) -> (f x)
  ; ((two f) x)->(((lambda (f) (lambda (x) (f (f x)))) f) x) -> ((lambda (x) (f (f x))) x) -> (f (f x))
  ; ((three f) x)->(((lambda (f) (lambda (x) (f (f (f x))))) f) x) -> ((lambda (x) (f (f (f x)))) x) -> (f (f (f x)))
  ; So, the addition procedure has to apply firstly a times f, i.e., ((a f) x) and then, another b times to that
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))




