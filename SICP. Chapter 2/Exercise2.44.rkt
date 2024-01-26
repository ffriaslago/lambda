#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (right-split painter n)
  
  (if (= n 0)
      painter
      
      (let ((smaller (right-split painter (- n 1))))
        
        (beside painter (below smaller smaller))))) ; Left: identity. upRight: (right-split n-1). bottomRight: (right-split n-1)

;right-split n
; ____________ ____________
;|            |            |
;|            |right-split |
;|            |  n-1       |
;|  identity  |------------|
;|            |right-split |
;|            |     n-1    |
;|____________|____________|


(define (corner-split painter n)
  
  (if (= n 0)
      painter
      
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          
          (beside (below painter top-left) ; upLeft: top-left. bottomLeft: identity.
                  (below bottom-right corner)))))) ; upRight: corner. bottomRight: bottom-right

;
; Actual Exercise
;

(define (up-split painter n)
  (if (= n 0)
      painter

      (let ((smaller (up-split painter (- n 1))))

        (below painter (beside smaller smaller))))) ; Bottom: identity. upLeft: (up-split n-1). upRight: (up-split n-1)

;up-split n
; ____________ ____________
;|            |            |
;|  up-split  |  up-split  | 
;|    n-1     |    n-1     |
;|-------------------------|
;|                         |
;|       identity          |
;|_________________________|

(paint (right-split einstein 4))
