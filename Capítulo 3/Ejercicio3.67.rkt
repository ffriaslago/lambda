#lang sicp

; Starting from original one, that would produce integers pairs with i<=j

; The idea is to add the streams changing the order before all the other pairs

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t)) ; Until here, the same
   
   (interleave ; First use of interleave
    
    (stream-map (lambda (x) 
                  (list x (stream-car t)))
                (stream-cdr s)) ; Order of s and t interchanged
    
    (interleave ; Sames as before. 
     (stream-map (lambda (x) 
                   (list (stream-car s) x))
                 (stream-cdr t))   
     (pairs (stream-cdr s) (stream-cdr t))))))

