#lang racket

;
; Section a)
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

;Face
;Smile
(define s18 (make-segment (make-vect 0.40 0.85) (make-vect 0.45 0.80)))
(define s19 (make-segment (make-vect 0.45 0.80) (make-vect 0.55 0.80)))
(define s20 (make-segment (make-vect 0.55 0.80) (make-vect 0.60 0.85)))
;Nose
(define s21 (make-segment (make-vect 0.50 0.85) (make-vect 0.50 0.90)))
;Eyes
(define s22 (make-segment (make-vect 0.40 0.95) (make-vect 0.45 0.95)))
(define s23 (make-segment (make-vect 0.55 0.95) (make-vect 0.60 0.95)))

(define seglist (list s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17))

; Solution
(segments->painter seglist)

;
; Section b)
;

(define (corner-splitv2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
           
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;
; Section c)
;

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    
    (let ((half (beside (flip-horiz quarter) quarter)))
      
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          
          (bottom (beside (bl painter) (br painter))))
      
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))