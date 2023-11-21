#lang sicp

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

(define x (list 'a 'b 'c))

(define y (list (cons '(a) '(a))))

(define y2 (cons (cons '(a) '(a)) (cons '(a) '(a))))

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

(define (memq item x)
  (cond ((null? x) false)
        ((equal? item (car x)) x)
        (else (memq item (cdr x)))))
 
(define (cycle? x) ; this serves for the cycles mades in Exercise 3.13
  (let ((initial-list x))
    (define (iter y) 
      (cond ((eq? y initial-list) #t) 
            ((null? y) #f) 
            (else (iter (cdr y))))) ; From Exercise 3.13, it is assume that cycles are made this way, it is reached a point where the pointer points to the object itself
    (if (null? x) 
        #f 
        (iter (cdr x)))))

(cycle? x) ; #f

(cycle? y) ; #f

(cycle? y2) ; #f

(cycle? z) ; #t

(eq? (cdddr z) z) ; #t

