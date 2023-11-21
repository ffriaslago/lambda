#lang sicp

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

z ; #0=(a b c . #0#). Endless list!

(last-pair z) ; Infinite loop! There is no last pair

; box-and-pointer diagram

;
;                                                    
;                                                              
;                                                             
;       ___ ___              ___ ___              ___ ___
;      |   |   |            |   |   |            |   |  /|
; x--->| · | · |----------->| · | · |----------->| · | / |
;      |___|___|            |___|___|            |___|/__|
;        |                    |                    |    
;        |                    |                    |     
;        |                    |                    |     
;        |                    |                    |       
;       \|/                  \|/                  \|/    
;       [a]                  [b]                  [c]

;
;         _____________________________________________                                                    
;        |                                             |        
;        |                                             |       
;       \|/ ___              ___ ___              ___ _|_
;      |   |   |            |   |   |            |   |   |
; z--->| · | · |----------->| · | · |----------->| · | · |
;      |___|___|            |___|___|            |___|___|
;        |                    |                    |    
;        |                    |                    |     
;        |                    |                    |     
;        |                    |                    |       
;       \|/                  \|/                  \|/    
;       [a]                  [b]                  [c]



