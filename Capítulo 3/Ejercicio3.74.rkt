#lang sicp

; sign-change-detector is a procedure that takes two values as arguments, compares the signs and returns 0, 1 or -1

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0)) ; sign of zero is positive for the calculation
; sense data is the stream from the sensor

; Let's analyse what make-zero-crossings does

; It builds a stream, being the first element the result of applying sign-change-detector to the first value of sense-data and 0
; The second element of zero-crossings will be the the result of applying sign-change-detector to the first two values of sense-data and so on

; Alternative code using

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      
      (cons-stream
       (apply proc (map stream-car argstreams)) ; apply is a primitive procedure that takes two arguments: a procedure and a list.
       ; it applies the procedure using the elements in the list as arguments
       ; map here returns a list with all the stream-car of the argstreams
       ; This means that the ⟨expression⟩ stream-car should be 0, as the procedure will be apply to it and (stream-car sense-data)
       
       (apply stream-map ; This will apply stream-map to a list consisting of
              (cons proc ; The original procedure 
                    (map stream-cdr ; The stream-cdr of the different arguments
                         argstreams))))))

; The correct order Alyssa wanted to implement is
; First to apply sign-change-detector to 0 and (stream-car sense-data)
; Second to apply sign-change-detector to (stream-car sense-data) and (stream-car (stream-cdr (sense-data))

; This means that the third argument in the following stream-map procedure has to be one step behind sense-data
; Since it has to be initialized with the value 0, the other part of the cons-stream should be the propoer sense-data stream

(define zero-crossings
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))