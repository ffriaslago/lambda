#lang racket

(define (map proc items)
  (if (null? items)
      empty
      (cons (proc (car items)) (map proc (cdr items)))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
			 (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
			 (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
	(lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
	(lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
	(lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
	(lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
	(lambda (r a) (tag (make-from-mag-ang r a))))
  ;
  ; From the Exercise!
  ;
  ;; added
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
	(lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; Explaining the error: The definition made for magnitude was for either rectangular or polar, there wasn't a magnitude procedure for the complex tag
; let ((proc (get op type-tags)) in this line inside apply-generic is the error

; Now, as it is fixed because the packages are complete

; From Figure 2.24, z can be understood as
;(define z (make-complex-from-real-imag 3 4))
; If it is evaluated
;(magnitude z)
; Line 98
;(apply-generic 'magnitude z) ; 1 time
;->(get 'magnitude '(complex))
;->magnitude
;(apply magnitude (('rectangular 3 4)))
;(magnitude ('rectangular (3 4))
;(apply-generic 'magnitude ('rectangular (3 4))) ; 2 times
;(sqrt (+ (square (real-part (3 4)))
;           (square (imag-part (3 4)))))
;5




