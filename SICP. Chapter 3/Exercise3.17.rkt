#lang sicp

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

(define (memq item x)
  (cond ((null? x) false)
        ((equal? item (car x)) x)
        (else (memq item (cdr x)))))

(define (new-count-pairs x)
  (let ((tracker nil))
    (define (counter x)      
      (if (or (not (pair? x)) (memq x tracker))
          0
          (begin
            (set! tracker (cons x tracker))
            (+ 1 (counter (car x)) (counter (cdr x))))))
    (counter x)))    

(define x (list 'a 'b 'c))

(define y (list (cons '(a) '(a))))

(define y2 (cons (cons '(a) '(a)) (cons '(a) '(a))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

(new-count-pairs x) ; 3

(new-count-pairs y) ; 3. Yay!

(new-count-pairs y2) ; 3. Yay!

;(new-count-pairs z) ; It doesn't count infinite loops, even if they consist in only three pairs, as it actually has infinite pairs. 

;(or (not (pair? y)) (memq y tracker)) ; #f
;(set! tracker (cons y tracker)) 
;tracker ; ((((a) a)))
;; +1
;(car y) ; ((a) a)
;(cdr y) ; () -> 0
;; (counter (car y))
;(or (not (pair? (car y))) (memq (car y) tracker)) ; #f
;(set! tracker (cons (car y) tracker)) 
;tracker ; (((a) a) (((a) a)))
;; +1
;(caar y) ; (a)
;(cdar y) ; (a)
;; (counter (caar y))
;(or (not (pair? (caar y))) (memq (caar y) tracker)) ; #f
;(set! tracker (cons (caar y) tracker))
;tracker ; ((a) ((a) a) (((a) a)))
;; +1
;(caaar y) ; a -> 0
;(cdaar y) ; () -> 0
;; (counter cdar y)
;(not (pair? (cdar y))) ; #f
;(cdar y) ; (a)
;tracker ; ((a) ((a) a) (((a) a)))
;(memq (cdar y) tracker) ; #f problem!!
;; It doesn't recognize (a) inside tracker
;(equal? (cdar y) (car tracker)) ; #f !!

; Let's try to compute the pairs of infinite loops after doing exercise 3.18

(define (cycle? x) 
  (let ((initial-list x))
    (define (iter y) 
      (cond ((eq? y initial-list) #t) 
            ((null? y) #f) 
            (else (iter (cdr y))))) 
    (if (null? x) 
        #f 
        (iter (cdr x)))))

(define (new-count-pairs-v2 x)
  (let ((tracker nil)
        (initial-list x))    
    (define (counter x)      
      (if (or (not (pair? x)) (memq x tracker))
          0
          (begin
            (set! tracker (cons x tracker))
            (if (eq? (cdr x) initial-list)
                (+ 1 (counter (car x)))                   
                (+ 1 (counter (car x)) (counter (cdr x)))))))
    (counter x)))

(new-count-pairs-v2 x) ; 3

(new-count-pairs-v2 y) ; 3. Yay!

(new-count-pairs-v2 y2) ; 3. Yay!

(new-count-pairs-v2 z) ; 3. Great!!








    