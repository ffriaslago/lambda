#lang racket

; Procedures from Ex.2.46

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; Procedures for Ex.2.48

(define (make-segment start-vec end-vec)
  (cons start-vec end-vec))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))