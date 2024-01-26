#lang sicp

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (cdr (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-front-ptr! deque (cons item (front-ptr deque)))))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (cons item '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque))))))

(define (rear-delete-deque! deque)
  (define (reverse l)
    (if (null? l)
        nil
        (append (reverse (cdr l)) (list (car l)))))
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        (else 
         (let ((inv-dq (cons (reverse (front-ptr deque)) (list (front-deque deque))))) ; ((d c b a) a)
           (set-front-ptr! inv-dq (cdr (front-ptr inv-dq))); ((c b a) a)
           (set-rear-ptr! deque (list (front-deque inv-dq))) ; ((c b a) c)
           (set-front-ptr! deque (reverse (front-ptr inv-dq)))))))  ; ((a b c) c)

(define (print-deque deque)
  (front-ptr deque))

(define q1 (make-deque))

(front-insert-deque! q1 'a)

(rear-insert-deque! q1 'b)

;(print-deque q1) ; (a b). OK

(front-insert-deque! q1 'c)

;(print-deque q1) ; (c a b). OK

;(front-ptr q1) ; (c a b). OK

;(rear-ptr q1) ; (b). OK

(front-delete-deque! q1)

;(print-deque q1) ; (a b). OK

(rear-insert-deque! q1 'c)
(rear-insert-deque! q1 'd)

(front-ptr q1)
(rear-ptr q1)

; Now q1 should be (a b c d), with front-ptr (a b c d) and rear-ptr (d)
(rear-delete-deque! q1)
; Our goal is to set q1 to be (a b c), with front-ptr (a b c) and rear-ptr (c)

(print-deque q1) ; (a b c). OK

(front-ptr q1) ; (a b c). OK

(rear-ptr q1) ; (c). OK

; The result is the one asked. However, I doubt rear-delete-deque! is accomplished in O(1) steps. I find difficult to check this.  