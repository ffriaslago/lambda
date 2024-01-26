#lang racket

;New definitions from the exercise's statement

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-numberscheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

; New version of apply-generic procedure

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc ; If there exists a procedure from the get function, no alternative
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for these types" (list op type-tags))))))
              (error "No method for these types"(list op type-tags))))))) ; Alternative to the if

; It has been updated the Scheme-number package with exponentiation, but not any other package

;
; Section A. What happens if we call exp with two complex numbers as arguments?
;

; It will generate an infinite loop, as apply-generic will call iteself in the new part (as it exists (put-coercion 'complex 'complex complex->complex). 

;
; Section B.
;

;It should work if it finds an operation inside the table for the arguments. Louis is not correct. 

;
; Section C.
;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags)))) ; it is avoided the case where the two types are the same
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error 
                          "No method for these types" (list op type-tags))))))
              (error "No method for these types"(list op type-tags))))))) 