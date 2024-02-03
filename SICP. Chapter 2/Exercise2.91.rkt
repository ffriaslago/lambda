#lang racket
(define (div-terms L1 L2) ; It returns two list, quotient and remainder
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      
      (let ((t1 (first-term L1)) ; each term has order and coefficient (value)
            (t2 (first-term L2)))
        
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1) ; L2 is the list of the divisor. It is assumed the list is ordered form highest to lowest order.
            
            (let ((new-c (div (coeff t1) (coeff t2))) ; coefficient of (first) element of the quotient
                  (new-o (- (order t1) (order t2)))) ; order of (first) element of the quotient
              
              (let ((rest-of-result ; division recursively. Multiply divisor by last term of quotient. Substract that from dividend and continue dividing by original divisor
                     (div-terms (sub-terms L1
                                           (mul-term-by-all-terms (make-term new-o new-c)
                                                                  L2))
                                L2)))
                ; As this is implemented recursively, it will be done until it is reached the condition (> (order t2) (order t1))->#f, so the quotient will be set to
                ; (the-empty-termlist) and remainer as the respective L1. From there, terms will be added, then, the returned lists quotient and remained can be constructed as:
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) ; quotient
                      (cadr rest-of-result)))))))) ; remainder

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) 
                      (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var: DIV-POLY"
             (list p1 p2))))