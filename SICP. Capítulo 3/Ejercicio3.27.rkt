#lang sicp


(define (memoize f) ; f is a procedure. So it takes a procedure as parameter
  
  (let ((table (make-table))) ; It creates a table
    
    (lambda (x) ; memoize returns a procedure, with one argument, a number
      (let ((previously-computed-result ; it looks up in the table for the value of the nth term of fibonacci (final argument is n)
             (lookup x table)))
        (or previously-computed-result ; If it is already computed, it is returned
            (let ((result (f x)))
              (insert! x result table) ; If not, it calculated with f, added to the table and returned
              result)))) ; Here it ends the lambda function


    ))

(define memo-fib
  (memoize
   
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (memo-fib (- n 1))
               (memo-fib (- n 2)))))) ; This is the procedure that it's the input for memoize
   ; Let's analyze this procedure
   ; It is a recursive one that takes a number and returns a value through memo-fib
   ; Example
   ;((lambda (n)
   ;   (cond ((= n 0) 0)
   ;         ((= n 1) 1)
   ;         (else 
   ;          (+ 8 4))))
   ; 7) ; 12
   
   ))

(memo-fib 3) ; is the result of applying the procedure returned by memoized when the input of memoized is a procedure with the fibonacci recursion

; After this analysis, would it work if we had simply defined memo-fib to be (memoize fib)

(define memo-fibv2
  (memoized 
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else 
            (+ (fib (- n 1))
               (fib (- n 2))))))))

;Will it work? yes, it will produce an answer, the correct one, but here (let ((result (f x)))), fib will call itself recursively and calculate the result without storing anything
;in the table, so it will be much slower or impossible to calculate for larger numbers, as it will just calculated without memoizing anything

; Regarding the number of steps, the original fib procedure was O(n^2). This new way has recursive calls, but avoids recalculation, so it can be seen,
; assuming lookup and insert are O(1) why this would be O(n)



