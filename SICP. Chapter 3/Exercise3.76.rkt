#lang sicp

(define (smooth s)
  (cons-stream
   (/ (+ (stream-car s) (stream-car (stream-cdr s))) 2)
   (smooth (stream-cdr s))))

(define (make-zero-crossings input-stream last-value)
  
  (let ((smooth-stream (smooth input-stream)))
    
    (cons-stream (sign-change-detector (stream-car smooth-stream) last-avpt)
                 (make-zero-crossings (stream-cdr smooth-stream) (stream-car smooth-stream)))))



  