#lang racket

; First way

(define (below painter1 painter2)
  
  (let ((split-point (make-vect 0.0 0.5)))
    
    (let ((paint-up
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.5)                             
                              (make-vect 0.0 1.0)))
          
          (paint-bottom
           (transform-painter painter2
                              (make-vec 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)))
      
      (lambda (frame)
        (paint-up frame)
        (paint-bottom frame)))))

; Second way

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))