#lang racket

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(sine 12.15)

; a) 12.15>0.1 -> (p (sine (/ 12.15 3.0))) -> (p (sine 4.05))
; 4.05>0.1 -> (p (p (sine (/ 4.05 3.0)))) -> (p (p (sine 1.35)))
; 1.35>0.1 -> (p (p (p (sine (/ 1.35 3.0))))) -> (p (p (p (sine 0.45))))
; 0.45>0.1 -> (p (p (p (p (sine (/ 0.45 3.0)))))) -> (p (p (p (p (sine 0.15)))))
; 0.15>0.1 -> (p (p (p (p (p (sine (/ 0.15 3.0))))))) -> (p (p (p (p (p (sine 0.05))))))
; 0.05 < 0.1 -> (p (p (p (p (p 0.05)))))
; 5 times

; b) (sine a). Order of growth in space and number of steps
; The angle a is divided by 3 every time p is applied, so the iteration stops if a/3^n<0.1 being n the number of steps
; Then, solving for n
; a/0.1 < 3^n
; log(a)-log(0.1) < n·log(3)
; n > (log(a)-log(0.1))/log(3)
; Since in this procedure, the order of growth in space and number of steps is the same, both relationed to n.
; It can be seen that both of them are O(log(a))
