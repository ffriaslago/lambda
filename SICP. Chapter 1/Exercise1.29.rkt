#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b)))) ; Here is is only adding one term to the rest of the sum from that term. So it calculates a term with (term a) and the next value (next a)

(define (integral f a b dx) ; f is the function, a and b the bounds and dx the infinitesimal values (parameter)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) ; term=f; a=(+ a (/ dx 2.0)); next=add-dx; b=b
     dx))

;(integral cube 0 1 0.01) ; 0.24998750000000042. (Real value: 0.25)

;(integral cube 0 1 0.001) ; 0.249999875000001. (Real value: 0.25)

(define (integralSimpson f a b n)
  (define (add-2h x) (+ x (/ (- b a) n) (/ (- b a) n)))
  (* (+ (f a)
        (* 4 (sum f (+ a (/ (- b a) n)) add-2h b))
        (* 2 (sum f (add-2h a) add-2h b))
        (if (even? n)
            (- (f b))
            (- (* 3 (f b))))) ; As in the definition of sum the condition is (> a b), if not corrected, this procedure adds more terms than it would be wanted, depending if it is n
     ; even or odd
     (/ (/ (- b a) n) 3)))

(integralSimpson cube 0 1 100) ; 1/4=0.25

(integralSimpson cube 0 1 1000) ; 1/4=0.25