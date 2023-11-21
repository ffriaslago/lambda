#lang sicp

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr  '()))
    (define (set-front-ptr! queue item) (set-car! queue item))
    (define (set-rear-ptr! queue item) (set-cdr! queue item))
    (define (empty-queue? queue) (null? (front-ptr queue)))
    (define (front-queue queue) (car (front-ptr)))
    (define (insert-queue! queue item) (let ((new-pair (cons item '())))
                                    (cond ((empty-queue? queue)
                                           (set-front-ptr! queue new-pair)
                                           (set-rear-ptr! queue new-pair)
                                           queue)
                                          (else (set-cdr! (rear-ptr queue) 
                                                          new-pair)
                                                (set-rear-ptr! queue new-pair)
                                                queue))))
    (define (delete-queue! queue) (cond ((empty-queue? queue)
                                          (error "DELETE! called with an empty queue" queue))
                                         (else (set-front-ptr! 
                                                queue 
                                                (cdr (front-ptr queue)))
                                               queue)))         
    (define (dispatch m)
      (cond ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!) 
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print) front-ptr)))
    dispatch))

(define q1 (make-queue))

(q1 'print)
