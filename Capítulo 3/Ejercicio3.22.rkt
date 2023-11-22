#lang sicp

(define (make-queue)
  
  (let ((front-ptr '())
        (rear-ptr  '()))

    (define (front-ptr-p) front-ptr)
    (define (rear-ptr-p) rear-ptr)
    
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    
    (define (empty-queue?) (null? front-ptr))
    
    (define (front-queue) (car front-ptr))
    
    (define (insert-queue! item) (let ((new-pair (cons item '())))
                                   
                                    (cond ((empty-queue?) (set-front-ptr! new-pair)
                                                          (set-rear-ptr! new-pair))
                                          
                                          (else (set-cdr! rear-ptr new-pair)
                                                (set-rear-ptr! new-pair)))))
    
    (define (delete-queue!) (cond ((empty-queue?) (error "DELETE! called with an empty queue"))
                                         (else (set-front-ptr! (cdr front-ptr)))))

    (define (print-queue) front-ptr)
    
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty-queue?)
            ((eq? m 'front-ptr) front-ptr-p)
            ((eq? m 'rear-ptr) rear-ptr-p)
            ((eq? m 'front) front-queue)
            ((eq? m 'insert) insert-queue!) 
            ((eq? m 'delete) delete-queue!)
            ((eq? m 'print) print-queue)))
    dispatch))

(define q1 (make-queue))

q1 ; is a procedure

((q1 'print)) ; returns ()

((q1 'insert) 'a) 

((q1 'print)) ; returns (a)

((q1 'insert) 'b) 

((q1 'print)) ; returns (a b)

((q1 'insert) 'c)

((q1 'print)) ; returns (a b c)

((q1 'delete)) 

((q1 'print)) ; returns (a b)

((q1 'delete))

((q1 'print)) ; returns (a)



