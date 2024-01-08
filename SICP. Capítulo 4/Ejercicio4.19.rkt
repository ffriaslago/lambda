#lang sicp

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

; It seems very difficult to implement a code in which Eva's view is the valid one, although it makes sense wanting both definitions to be simultaneous

; I'd stick with Alyssa's view as it seems more simple.