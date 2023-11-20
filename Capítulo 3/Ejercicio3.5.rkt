#lang racket

; Founded solution for this issue
(require srfi/27)

(define (random-in-range low high)
  (+ low (* (random) (- high low))))

(define (square x) (* x x))

; Original code
;(define (random-in-range low high)
;  (let ((range (- high low)))
;    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (circle-region x1 x2 y1 y2) ; Assuming the circle region is tangent to the rectangle in all the four sides
  (let ((xrand (random-in-range x1 x2))
        (yrand (random-in-range y1 y2))
        (xcent (/ (+ x1 x2) 2))
        (ycent (/ (+ y1 y2) 2))
        (radius (/ (- x2 x1) 2)))   
    (not (> (+ (square (- xrand xcent)) (square (- yrand ycent))) (square radius)))))

(define (estimate-integral P x1 x2 y1 y2 trials)
    (monte-carlo trials (lambda() (P x1 x2 y1 y2))))

(define (estimate-pi trials)
  (* 4.0 (estimate-integral circle-region -1.0 1.0 -1.0 1.0 trials))) ; It returns the number of times a point inside the rectangle of corners (-1,-1) (1,1) falls into the unit-circle
; The area of that rectangle is 4, so if this number is multiplied by 4, it would give an estimation of the area of the unit circle, which should be pi

(estimate-pi 100000) ; 3.1452. Great!

; The problem is that random appears to work only with integers 
