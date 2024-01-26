#lang racket

; From Exercise2.14 we can conclude that
; Dividing and multuplying add tolerances
; Adding averages them
; Substracting results in a much higher tolerance
; Reciprocate preserves it

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

; par1
; one addition, one multiplication and one division. If we start with 2 percent in r1 and r2, we would end up with a 6 percent tolerance

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

; par2
; one addition, three reciprocals. If we start with 2 percent in r1 and r2, we would end up with a 2 percent tolerance, so it is better