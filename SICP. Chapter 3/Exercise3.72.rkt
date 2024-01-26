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

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))


(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (merge-weighted s1 s2 weight) ; s1 and s2 are streams. weight is a procedure
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           ; Until here, same procedure
           (cond ((<= (weight s1car) (weight s2car)) ; It is computed the weight.                  
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight))))))))
                 ; What to do in the case they have the same weight? For example, for section A. (1 9) (2 8) (3 7) (4 6) (5 5) all will have the same weight.
                 ; Ideally, the best order is to put them in the same order as they are written, so first group
                 

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t)) ; First element of each stream. It is assumed that this pair is the one with the lowest weight
   
   (merge-weighted ; instead of interleave, because now a precise order is wanted
    
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t)) ; The first stream is the same one as for interleave 
    
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight) ; Second one is the merge-weighted procedure with the cdrs
    
    weight)))

(define (square x) (* x x))

(define (weight372 pair)
  (+ (square (car pair)) (square (cadr pair))))

(define s372 (weighted-pairs integers integers weight372))

(define (stream-filterv3 pred stream) ; Alternative version to take two consecutive elements
  (cond ((stream-null? stream) 
         the-empty-stream)
        
        ((pred (stream-car stream) (stream-car (stream-cdr stream)) (stream-car (stream-cdr (stream-cdr stream))))
         (cons-stream
          ; Improved line from Exercise 3.71
          (list (weight372 (stream-car stream)) (stream-car stream) (stream-car (stream-cdr stream)) (stream-car (stream-cdr (stream-cdr stream))))
          (stream-filterv3 
           pred
           (stream-cdr stream))))
        
        (else (stream-filterv3 
               pred 
               (stream-cdr stream)))))

(define 372Stream
  (stream-filterv3
   (lambda (x y z)
     (and (= (weight372 x) (weight372 y))
          (= (weight372 y) (weight372 z))))
   s372))

(stream-ref 372Stream 0) ; (325 (1 18) (6 17) (10 15))
(stream-ref 372Stream 1) ; (425 (5 20) (8 19) (13 16))
(stream-ref 372Stream 2) ; (650 (5 25) (11 23) (17 19))
(stream-ref 372Stream 3) ; (725 (7 26) (10 25) (14 23))
(stream-ref 372Stream 4) ; (845 (2 29) (13 26) (19 22))
(stream-ref 372Stream 5) ; (850 (3 29) (11 27) (15 25))
; ...

