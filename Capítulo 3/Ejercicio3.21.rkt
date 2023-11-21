#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

; Exercise 3.21

;(define q1 (make-queue))
; (cons '() '()) -> (())
; (car q1) -> ()
; (cdr q1) -> ()

;(insert-queue! q1 'a) ; ((a) a)
; newpair->(cons 'a '()) ; '(a)
; q1 is empty
;(set-front-ptr! '(()) '(a)) -> (set-car! '(()) '(a)) -> q2=((a))
;(set-rear-ptr! '(()) '(a)) -> (set-cdr! '(()) '(a)) -> q2=((a) a)
;(front-ptr q1) -> '(a)
;(rear-ptr q1) -> '(a)

;                                                             
;        ___ ___                      
;       |   |   |              
; q1--->| · | · |
;       |___|___|            
;         |   |                                      
;         |   |                                     
;         |   |                                    
;         |   |                                     
;        \|/ \|/                      
;       |   |  /|              
;       | · | / |
;       |___|/__|
;         |
;         |
;         |
;        \|/
;        [a]

;(insert-queue! q1 'b) ; ((a b) b)
; newpair->'(b)
; q1 is not empty
; (set-cdr! (rear-ptr q1) '(b)) -> (set-cdr! (cdr q1) '(b)) -> (set-cdr! '(a) '(b)) -> '(a b), but q1 transforms into '((a b) a b) (See diagram
; (set-rear-ptr! q1 '(b)) -> (set-cdr! q1 '(b)) -> (set-cdr! ((a b) a b) '(b)) -> '((a b) b)

;                                                             
;        ___ ___                      
;       |   |   |              
; q1--->| · | · |
;       |___|___|            
;         |   |                                      
;         |   |                                     
;         |   |                                    
;         |   |                                     
;        \|/ \|/                      
;       |   |  /|              
;       | · | / |
;       |___|/__|
;         |   | 
;         |   |-----------> (rear-ptr q1) is '(a), where (car '(a)) is a and (cdr '(a)) is '(). Using set-cdr!, changes '(). If it is replaced by '(b), which has
;         |                 (car '(b)) b and (cdr '(b)) '(b), then, the diagram transforms into
;        \|/
;        [a]

;                                                             
;        ___ ___                      
;       |   |   |              
; q1--->| · | · |------------|
;       |___|___|            |
;         |                  |                       
;         |                  |                      
;         |                  |                    
;         |                  |                      
;        \|/ ___          __\|/__           
;       |   |   |        |   |  /|   
;       | · | · |------->| · | / |
;       |___|___|        |___|/__|   
;         |                |
;         |                |
;         |                |
;        \|/              \|/
;        [a]              [b]

; This is why front-ptr changes too!, because the first pair now does not have an empty cdr, as the list has two elements!!

;(delete-queue! q1) ; ((b) b)
; This is easy to understand, the queue reflects the same diagram as the first one, but with b instead of a
;                                                             
;        ___ ___                      
;       |   |   |              
; q1--->| · | · |--------------|
;       |___|___|              |
;         |                    |                       
;         |                    |                      
;         |----------------|   |                    
;                          |   |                      
;        ___ ___          \|/ \|/          
;       |   |   |        |   |  /|   
;       | · | · |------->| · | / |
;       |___|___|        |___|/__|   
;         |                |
;         |                |
;         |                |
;        \|/              \|/
;        [a]              [b]

;(delete-queue! q1) ; (() b)
; q1 is not empty
; (set-front-ptr! q1 (cdr (front-ptr queue))) -> (set-car! q1 (cdr '(b))) -> (set-car! q1 '()) -> q1 is (() b)
; rear-ptr is not changed, that's why it appears b.

;        ___ ___                      
;       |   |   |              
; q1--->| · | · | It is not connected to anything
;       |___|___|                                    
;                                                                                                    
;        ___ ___          ___ ___          
;       |   |   |        |   |  /|   
;       | · | · |------->| · | / |
;       |___|___|        |___|/__|   
;         |                |
;         |                |
;         |                |
;        \|/              \|/
;        [a]              [b]


; As seen in the previous examples, the front pointer points to the first element of the list. As the list is concatenated, it will show all the elements until the last one.
; The procedure front-queue returns the car of front-ptr, i.e., the list it points

(define (print-queue queue)
  (front-ptr queue))

(define q2 (make-queue))

(print-queue q2) ; ()

(insert-queue! q2 'a)

(print-queue2 q2) ; (a)

(insert-queue! q2 'b)

(print-queue2 q2) ; (a b)

(insert-queue! q2 'b)

(insert-queue! q2 'b)

(print-queue2 q2) ; (a b b b)





        
