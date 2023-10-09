#lang racket

(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

; Substitution model
; (+ 4 5)
; (inc (+ (dec 4) 5 ))
; (inc (+ 3 5))
; (inc 8)
; 9

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))

; Substitution model
; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; 9

; Both are recursive processed, as they are a chain of deferred operations