#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

; count-leaves procedure as an accumulation

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (cond ((null? x) 0)
                           ((not (pair? x)) 1)
                           (else (count-leaves x)))) 
                   t)))

; It's an accumulation where each item of the tree is analyzed
; It it's empty, nothig is added
; If it's not a pair, it is added 1, since it is a leave
; If it's a pair, i.e, not a leave, it uses recursion to calculate that tree
; Then, the sequence of this accumulation would be a list with the number of leaves for each item

(define x (cons (list 1 2) (list 3 4)))

(define t (list x x))

t

(car t)

(count-leaves t) ; 8

; Let's see what is the final sequence
;(count-leaves '(((1 2) 3 4) ((1 2) 3 4)))
;(accumulate +
;            0
;            (map (lambda (x)
;                     (cond ((null? x) 0)
;                           ((not (pair? x)) 1)
;                           (else (count-leaves x)))) 
;                   '(((1 2) 3 4) ((1 2) 3 4))))
;(map (lambda (x)
;       (cond ((null? x) 0)
;             ((not (pair? x)) 1)
;             (else (count-leaves x)))) 
;     '(((1 2) 3 4) ((1 2) 3 4)))
;(cons (lambda (x) ... '((1 2) 3 4))
;      (map (lambda (x) ...) '((1 2) 3 4)))
;(lambda (x) ... '((1 2) 3 4))
;-->(count-leaves '((1 2) 3 4))
;   (accumulate +
;               0
;               (map (lambda (x)
;                        (cond ((null? x) 0)
;                              ((not (pair? x)) 1)
;                              (else (count-leaves x)))) 
;                      '((1 2) 3 4)))
;   (map (lambda (x)
;          (cond ((null? x) 0)
;                ((not (pair? x)) 1)
;                (else (count-leaves x)))) 
;        '((1 2) 3 4))
;   (cons (lambda (x) ... '(1 2))
;         (map (lambda (x) ...) '(3 4)))
;   (lambda (x) ... '(1 2))
;----->(count-leaves '(1 2))
;      (accumulate +
;                  0
;                  (map (lambda (x)
;                         (cond ((null? x) 0)
;                               ((not (pair? x)) 1)
;                               (else (count-leaves x)))) 
;                       '(1 2)))
;      (map (lambda (x)
;             (cond ((null? x) 0)
;                   ((not (pair? x)) 1)
;                   (else (count-leaves x)))) 
;           '(1 2))
;      (cons (lambda (x) ... 1)
;            (map (lambda (x) ...) 2))
;      (cons 1
;            (map (lambda (x) ...) 2))
;      (cons 1 1)
;<-----'(1 1)
;   (cons '(1 1)
;         (map (lambda (x) ...) '(3 4)))
;   ...
;   (cons '(1 1)
;         '(1 1))
;<--'(1 1 1 1)
;(cons '(1 1 1 1)
;      (map (lambda (x) ...) '((1 2) 3 4)))
;...
;(cons '(1 1 1 1)
;      '(1 1 1 1))
;'(1 1 1 1 1 1 1 1)
;(accumulate +
;            0
;            '(1 1 1 1 1 1 1 1))



