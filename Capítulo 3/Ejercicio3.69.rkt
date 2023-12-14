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

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

; Let's start from the definition of pairs st

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; The objective is to write a procedure that adds to (pairs t u) all the elements from s

(define (triples s t u)
  
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u)) ; As seen in exercise 3.68, this is absolutely necessary
   
   (interleave ; Then, call to interleave, as in pairs

    (stream-map (lambda (x)
                  (append (list (stream-car s)) x)) ; The difference here is because there is a list already created in pairs, so it is needed to use append
                (pairs (stream-cdr t) (stream-cdr u)))

    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))) ; Recursive call to triples

(define s1 (triples integers integers integers)) ; s1 is the stream with integer triples (i, j, k) such as i<=j<=k

(define s2 ; using stream-filter, it is obtained the desired stream
  (stream-filter
   (lambda (x)  ; This lambda function has as input the stream-car of s1, which is a list of three elements, that's why the procedures car, cadr and caddr are used
     (= (+ (* (car x) (car x)) (* (cadr x) (cadr x)))
        (* (caddr x) (caddr x))))   
   s1))

(stream-ref s2 0) ; (3 4 5)

(stream-ref s2 1) ; (6 8 10)

(stream-ref s2 2) ; (5 12 13)