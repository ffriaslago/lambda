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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define s1 (pairs integers integers))

(stream-ref s1 0) ; (1 1)

(stream-ref s1 1) ; (1 2)

(stream-ref s1 2) ; (2 2)

(stream-ref s1 3) ; (1 3)

(stream-ref s1 4) ; (2 3)

(stream-ref s1 5) ; (1 4)

(stream-ref s1 6) ; (3 3)

(stream-ref s1 7) ; (1 5)

(stream-ref s1 8) ; (2 4)

(stream-ref s1 9) ; (1 6)

(stream-ref s1 10) ; (3 4)

(stream-ref s1 11) ; (1 7)

;    1   2   3   4   5
; 1 p0
; 2 p1  p2
; 3 p3  p4  p6 
; 4 p5  p8  p10 p14
; 5 p7  p12 p18 p22 p30
; 6 p9  p16 p26 p38 p46
; 7 p11 p20 p34 p54
; 8 p13 p24 p42 p70

; First conclusion, the pair (1, n) with n>1 is the 2*n-3 pair. So the pair (1,100) will be the 197th pair (counting from 0).

; Second conclusion, the pair (n,n) has the position 2^n-2, So the pair (100, 100) will be the 2^100-2th pair (counting from 0)

; Third. (1 2) -> p1. (2 3) -> p4. (3 4) -> p10. (4 5) -> p22. (5 6) -> p46.
; The differences in position are: 3, 6, 12, 24. So next one, the pair (6 7) would be p94, as it is 46+48.
; So the series follows: 1(1), 4(2), 10(3), 22(4), 46(5), 94(6), 190(7), 382(8)...

; Then, the general term of the upper series is a_n=2^n+2^(n-1)-2

; So, the pair (99, 100) will be in the 2^99+2^98-2 position
