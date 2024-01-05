;#lang racket ; INFINITE LOOP. It evaluates the two operands, 0 and (p), to get the arguments. (p) gives (p) again and again, in an infinite loop.

;#lang lazy ; As x=0, it gives 0 without evaluating (p) as it is not needed. NO INFINITE LOOP

(define (p) (p))

(define (test x y) 
  (if (= x 0) 
      0 
      y))

(test 0 (p))