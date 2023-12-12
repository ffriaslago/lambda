#lang sicp

; All the auxiliary definitions of subsection 3.5.1

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) 
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr
                         argstreams))))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

; Actual Exercise 3.52

(define sum 0)

sum ; Value: 0. Just the definition and it sets the initial value to 0.

(define (accum x)
  (set! sum (+ x sum))
  sum)

sum ; Value: 0. The procedure accum is defined. It changes sum when called

(define seq 
  (stream-map 
   accum ; call to accum!
   (stream-enumerate-interval 1 20)))

sum ; Value: 1. As seq is defined, accum is executed. With the stream-enumerate-interval, only the first element of the interval is used now, so it is executed (set! sum (+ 1 sum))

(define y (stream-filter even? seq))

sum ; Value: 6. First element: 1, not even. Next element.
    ;           Accum executes for second time, sum is 3, seq elements are 1 and 3
    ;           Second element: 3, not even. Next element.
    ;           Accum executes, sum is 6, seq elements are 1, 3 and 6
    ;           Third element: 6, even. No more calculations.

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

sum ; Value: 10. Seq is 1, 3 and 6. None of the elements is divisible by 5. Next element
    ;            Accum executes, sum is 10, seq elements are 1, 3, 6 and 10.
    ;            10 is divisible by 5. No more calculations. 

(stream-ref y 7)

; Printed response: 136

sum ; Value: 136. Let's calculate seq until y has 8 elements.
    ; 1, 3, 6 OK, 10, 15, 21, 28 OK, 36 OK, 45, 55, 66 OK, 78 OK, 91, 105, 120 OK, 136 OK

(display-stream z)

; Printed response:
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210 done

sum ; Value: 210. stream-enumerate-interval goes until 20, so now seq is calculated completely
    ; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210.



; Now, using (delay <exp>) as (lambda () <exp>). There is no memoization, so the value of sum is changed by accum.
; Then, after the definition of z, the results are already different because of the different value of sum.