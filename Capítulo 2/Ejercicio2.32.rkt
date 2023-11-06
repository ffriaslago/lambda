#lang racket

(define (subsets s)
  (if (null? s)
      (list empty)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))

;Let's deconstruct it

;(subsets '(1 2 3)). Known, (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;rest<-(subsets '(2 3))

;   (subsets '(2 3)). Known, (() (3) (2) (2 3))
;   rest<-(subsets '(3))

;      (subsets '(3)). Known, (() (3))
;      rest<-(subsets '())->(by definition of the procedure)->'(())
;      (append '(()) (map <??> '(()))
;      <??> is a procedure that applied to '(()) gives '(3), being '(3) the argument of the original function
;      <??> needs to be a lambda function
;      (lambda (x) (cons s x)) because (cons number empty) = (number), (cons empty number)=(() number)
;      (append '(()) (map (lambda (x) (cons '(3) x)) '(()))
;      (append '(()) '(((3)))) This is not correct, too many lists, the correct instruction should be (cons 3 x)
;      The change needed in the procedure is
;      (lambda (x) (cons (car s) x))
;      (append '(()) (map (lambda (x) (cons 3 x)) '(()))
;      (append '(()) '((3)))
;      '(() (3))    

;   (append '(() (3)) (map (lambda (x) (cons (car '(2 3)) x)) '(() (3))))
;   The only interesting thing is to add '(2) to the existing subsets to generate the subsets '(2) and (2 3), which is consisten with the previous step
;   and the decision of implementing (car s) instead of s inside lambda function
;   (append '(() (3)) (map (lambda (x) (cons (car '(2 3)) x))'(() (3))))
;   (append '(() (3)) (map (lambda (x) (cons 2 x))'(() (3))))
;   (append '(() (3)) '((2) (2 3)))
;   '(() (3) (2) (2 3))

;(append '(() (3) (2) (2 3)) (map (lambda (x) (cons (car '(1 2 3)) x)) '(() (3) (2) (2 3))))
;(append '(() (3) (2) (2 3)) (map (lambda (x) (cons 1 x)) '(() (3) (2) (2 3))))
;(append '(() (3) (2) (2 3)) '((1) (1 3) (1 2) (1 2 3)))
;'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)) Final answer

;The logic behind this procedure is that to compute all the subsets of a given set you compute
; First, all subsets excluding the first number, in this case, 1
; Second, you add that first number to all the subsets you had
; Third: Apply recursively






