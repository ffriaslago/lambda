#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; Option 1

(define x (list 'a 'b 'c))

x ; (a b c)

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

(count-pairs x) ; 3

; Option 2

(define y (list (cons '(a) '(a))))

y ; (((a) a))

;
;                                [a]
;       ___ ___                  /|\
;      |   |  /|                  |
; y--->| · | / |                 _|_ ___
;      |___|/__|                |   |  /|                              
;        |                      | · | / |                                                             
;        |                      |___|/__|                                                                     
;        |                       /|\ /|\               
;        |                        |   |               
;        |                       _|_ _|_               
;        |                      |   |   |            
;        |--------------------->| · | · |
;                               |___|___|

(count-pairs y) ; 4

; Option 3

(define y2 (cons (cons '(a) '(a)) (cons '(a) '(a))))

y2 ; (((a) a) (a) a)

; Box-and-pointer diagram for new z1, after (set-to-wow! z1) -> (set-car! (car z1) 'wow) -> (set-car! x 'wow) 
;
;
;       ___ ___
;      |   |   |
; y2-->| · | · |
;      |___|___|                                            
;        |   |                                                   
;        |   |                                                            
;       \|/ \|/              
;      |   |   |            
;      | · | · |
;      |___|___|
;        |   |
;        |   |
;       \|/ \|/
;      |   |  /|            
;      | · | / |
;      |___|/__|            
;        |                                                                                                                                                                               
;        |                                                           
;       \|/                                                     
;       [a]                 

(count-pairs y2) ; 7


; Option 4

;(define (make-cycle x)
;  (set-cdr! (last-pair x) x)
;  x)
;
;(define (last-pair x)
;  (if (null? (cdr x))
;      x
;      (last-pair (cdr x))))
;
;(define z (make-cycle (list 'a 'b 'c)))
;
;(count-pairs z) ; Never return at all

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