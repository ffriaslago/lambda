#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; Procedures from Ex.2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; Procedures from Ex.2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

; Procedures from Ex.2.48

(define (make-segment start-vec end-vec)
  (cons start-vec end-vec))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

; Auxiliary procedure from the book

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; Procedure for this exercise

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line ; Note: the procedure draw-line does not exist, we work under the assumption it exists
        ((frame-coord-map frame) (start-segment segment)) ; Initial point for draw-line
        ((frame-coord-map frame) (end-segment segment)) ; End point for draw-line
        ))
     segment-list)))
;
; Section A
;
; We need four segments:
; 1. From the frame origin (0 0) to the bottom right (1 0)
; 2. From the bottom right (1 0) to the opposite corner to origin (1 1)
; 3. From the opposite corner (1 1) to the upper left (0 1)
; 4. From the upper left (0 1) to the origin (0 0)

(define seg1 (make-segment (make-vect 0 0) (make-vect 1 0)))
(define seg2 (make-segment (make-vect 1 0) (make-vect 1 1)))
(define seg3 (make-segment (make-vect 1 1) (make-vect 0 1)))
(define seg4 (make-segment (make-vect 0 1) (make-vect 0 0)))

(define seglistA (list seg1 seg2 seg3 seg4))

; Solution
(segments->painter seglistA)

;
; Section B
;
; We need two segments:
; 1. From the frame origin (0 0) to opposite corner (1 1)
; 2. From the bottom right (1 0) to the upper left (0 1)

(define seg5 (make-segment (make-vect 0 0) (make-vect 1 1)))
(define seg6 (make-segment (make-vect 1 0) (make-vect 0 1)))

(define seglistB (list seg5 seg6))

; Solution
(segments->painter seglistB)

;
; Section C
;
; We need four segments:
; 1. From half of the first side (0.5 0) to the half of second side (1 0.5)
; 2. From half of the second side (1 0.5) to the half of third side (0.5 1)
; 3. From half of the third side (0.5 1) to the half of fourth side (0 0.5)
; 4. From half of the fourth side (0 0.5) to the half of first side (0.5 0)

(define seg7 (make-segment (make-vect 0.5 0) (make-vect 1 0.5)))
(define seg8 (make-segment (make-vect 1 0.5) (make-vect 0.5 1)))
(define seg9 (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))
(define seg10 (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))

(define seglistC (list seg7 seg8 seg9 seg10))

; Solution
(segments->painter seglistC)

;
; Section D
;

; Interior legs
(define s1 (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3)))
(define s2 (make-segment (make-vect 0.6 0.0) (make-vect 0.5 0.5)))

;Exterior legs
(define s3 (make-segment (make-vect 0.25 0.0) (make-vect 0.35 0.55)))
(define s4 (make-segment (make-vect 0.75 0.5) (make-vect 0.60 0.50)))

;Head
(define s5 (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00)))
(define s6 (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85)))
(define s7 (make-segment (make-vect 0.65 0.85) (make-vect 0.60 1.00)))
(define s8 (make-segment (make-vect 0.60 0.70) (make-vect 0.65 0.85)))

;Right arm
(define s9 (make-segment (make-vect 0.60 0.50) (make-vect 1 0.2)))
(define s10 (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70)))
(define s11 (make-segment (make-vect 0.75 0.70) (make-vect 1 0.4)))

;Left arm
(define s12 (make-segment (make-vect 0.35 0.55) (make-vect 0.30 0.65)))
(define s13 (make-segment (make-vect 0.40 0.70) (make-vect 0.30 0.70)))
(define s14 (make-segment (make-vect 0.30 0.65) (make-vect 0.15 0.45)))
(define s15 (make-segment (make-vect 0.30 0.70) (make-vect 0.15 0.65)))
(define s16 (make-segment (make-vect 0.15 0.45) (make-vect 0 0.70)))
(define s17 (make-segment (make-vect 0.15 0.65) (make-vect 0 0.85)))

(define seglistD (list s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17))

; Solution
(segments->painter seglistD)
  

