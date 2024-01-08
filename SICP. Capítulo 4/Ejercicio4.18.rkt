#lang sicp

(define (solve f y0 dt)
  
  (define y (integral (delay dy) y0 dt))
  
  (define dy (stream-map f y))
  
  y)

; Strategy 1

; From this

(lambda ⟨vars⟩
  (define u ⟨e1⟩)
  (define v ⟨e2⟩)
  ⟨e3⟩)

; to this

(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a ⟨e1⟩)
          (b ⟨e2⟩))
      (set! u a)
      (set! v b))
    ⟨e3⟩))

; Then solve would be

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

; If the let are substitued by lambda functions, the result will be

(define (solve f y0 dt)
  ((lambda (y dy)
     ((lambda (a b)
        (set! y a) ; here, at this point, both y and dy are unassigned, so this does not work 
        (set! dy b))
      (integral (delay dy) y0 dt)
      (stream-map f y))
     y)
  '*unassigned
  '*unassigned))

; Strategy 2

; From this

(lambda ⟨vars⟩
  (define u ⟨e1⟩)
  (define v ⟨e2⟩)
  ⟨e3⟩)

; to this

(lambda ⟨vars⟩
  (let ((u '*unassigned*)
        (v '*unassigned*))
      (set! u ⟨e1⟩)
      (set! v ⟨e2⟩))
    ⟨e3⟩)

; Then solve would be

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
      (set! y (integral (delay dy) y0 dt))
      (set! dy (stream-map f y)))
    y)

; Using the lambda functions

(define (solve f y0 dt)
  ((lambda (y dy)
     (set! y (integral (delay dy) y0 dt))
     (set! dy (stream-map f y))
     y)
   '*unassigned
   '*unassigned))

; Which can work as, simulateuosly, indeed. 

      
        