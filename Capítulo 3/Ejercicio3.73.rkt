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

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (integral integrand initial-value dt) ; From the book
  (define int
    (cons-stream 
     initial-value
     (add-streams (scale-stream integrand dt)
                  int)))
  int)

; Exercise 3.73

; Formula: v = v0+1/C int_0^t i dt + Ri
; Using the integral procedure it can be calculated: v0+1/C int_0^t i dt
; Using scale-stream it can be calculated: Ri
; It will be used add-streams to add both

(define (RC R C dt)
  (lambda (i v0)
    (add-streams 
     (integral (scale-stream i (/ 1.0 C)) v0 dt)
     (scale-stream i R))))

(define RC1 (RC 5 1 0.5))

(define test-signal (cons-stream 0 (cons-stream 0.5 (cons-stream 1 test-signal))))

(define streamtest (RC1 test-signal 10))

(stream-ref streamtest 0) ; 10

(stream-ref streamtest 1) ; 12.5

(stream-ref streamtest 2) ; 15.25

(stream-ref streamtest 3) ; 10.75

