#lang sicp

(define (make-zero-crossings input-stream last-value)
  
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0))

; Let's reconstruct what the procedure does now

; 1. avpt is the average of 0 and (stream-car sense-data)
; 2. avpt is the average of the former average and (stream-car (stream-cdr sense-data)). Wrong!
; What Alyssa wanted is to do the average with the previous value, not the previous average!
; However, as it is hinted, it is needed to save the value of avpt as the sign-change-detector procedure has to be applied to the smoothed signal
; Let's fix it

(define (make-zero-crossingsv2 input-stream last-sense-data-value last-avpt)
  
  (let ((avpt (/ (+ (stream-car input-stream) last-sense-data-value) 2)))
    
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream) avpt))))