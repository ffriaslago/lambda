#lang sicp

(
 (lambda (n) ; The argument for this lambda function is a number, the number we want to calculate the factorial. 10 in this case
   
   ((lambda (fact)
      (fact fact n))
    
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))

    )
   )
 10)

; If this is run, it gives 3628800, so it computes factorials indeed

; Let's do some substitution to understand

; First step

((lambda (fact) (fact fact 10))    
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))

; Second Step

(
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 ; The first argument ft of this lambda function is itself, the second argument k is number 10 
 (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
 10)

(if (= 10 1)
    1
    (* 10 ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))) (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))) 9)))

(* 10
   ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
    9))

(* 10
   (* 9
      ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
       (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
       8)))

; ...

(* 10
   (* 9
      (* 8
         (* 7
            (* 6
               (* 5
                  (* 4
                     (* 3
                        (* 2
                           ((lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
                            (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
                            1))))))))))

(* 10
   (* 9
      (* 8
         (* 7
            (* 6
               (* 5
                  (* 4
                     (* 3
                        (* 2
                           (* 1))))))))))

; and so on

; Following the same reasoning it can be implemented the fib function

(
 (lambda (n)
   
   ((lambda (fib)
      (fib fib n))
    
    (lambda (fn k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (fn fn (- k 1))
                     (fn fn (- k 2))))))))
 10)

; This gives 55

;
; Section B
;

; This program is based in that it keeps substracting 1 to the original number and when it reaches 0, if
; original number was even, it will generate even? 0, which is true
; original number was odd, it will generate odd? 0, which is false


(define (f x)
  
  ((lambda (even? odd?)
     (even? even? odd? x))
   
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1)))) ; Alternating
   
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1)))))) ; Same order as the outer lambda

; Let's check with an example
; Example

(f 2)

; First step, outer lambda

((lambda (even? odd?)
     (even? even? odd? 2))
   
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1)))))

; Second step, inner lambda

( (lambda (ev? od? n)
    (if (= n 0) 
        true 
        (od? ev? od? (- n 1))))
     
  (lambda (ev? od? n)
    (if (= n 0) 
        true 
        (od? ev? od? (- n 1))))
   
  (lambda (ev? od? n)
    (if (= n 0) 
        false 
        (ev? ev? od? (- n 1))))
  2)

; 2 is not 0, so it triggers (od? ev? od? (- n 1))
; First calculation

( (lambda (ev? od? n)
    (if (= n 0) 
        false 
        (ev? ev? od? (- n 1))))

  (lambda (ev? od? n)
    (if (= n 0) 
        true 
        (od? ev? od? (- n 1))))
  
  (lambda (ev? od? n)
    (if (= n 0) 
        false 
        (ev? ev? od? (- n 1))))
  1)

; 1 is not 0, so it triggers (ev? ev? od? (- n 1))
; Second calculation, 0 is reached, we need to have the first one for it to be true

( (lambda (ev? od? n)
    (if (= n 0) 
        true 
        (od? ev? od? (- n 1))))

  (lambda (ev? od? n)
    (if (= n 0) 
        true 
        (od? ev? od? (- n 1))))

  (lambda (ev? od? n)
    (if (= n 0) 
        false 
        (ev? ev? od? (- n 1))))
  0)

; (= 0 0) -> true

