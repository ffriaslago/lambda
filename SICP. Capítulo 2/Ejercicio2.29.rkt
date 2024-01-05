#lang racket

; A

(define (make-mobile left right) (list left right))
;(define (make-mobile left right) (cons left right))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (car (cdr mobile)))
;(define (right-branch mobile) (cdr mobile))

(define (make-branch length structure) (list length structure))
;(define (make-branch length structure) (cons length structure))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (car (cdr branch)))
;(define (branch-structure branch) (cdr branch))

(define mob1 ;'((5 1) (6 2)). It is a pair
  (make-mobile (make-branch 5 1)
	       (make-branch 6 2)))

(left-branch mob1) ; '(5 1)

(cdr mob1) ; '((6 2)). Important to add a car procedure before the cdr, to have the real list

(right-branch mob1) ; '(6 2)

(define mob2 ; '((5 1) (6 ((5 1) (6 2)))). It is a pair!!
  (make-mobile (make-branch 5 1)
	       (make-branch 6 mob1)))

;; B
;
(define (total-weight mobile) ; A mobile has two branches, the weight of the mobile is the sum of the weights
  (if (not (pair? mobile))
      mobile
      (+ (total-weight (branch-structure (left-branch mobile))) ; Weight of left branch
           (total-weight (branch-structure (right-branch mobile)))))) ; Weight of right branch

; Branch-structure returns a weight (number) or a pair (a mobile), which has two branches. If it's not a pair, the only return parameter is the weight

(total-weight mob1) ; 3:1+2 (the length does not matter yet)

(total-weight mob2); 4=1+3(1+2)
;
;; C
;
(define (torque branch) ; The definition of the torque is straight-forward, i.e., the length times total weight of a branch
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile) 
  (if (not (pair? mobile)) 
      true 
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile))) ; first condition, with the top branches
           (balanced? (branch-structure (left-branch mobile))) ; To consider submobiles in left-branch
           (balanced? (branch-structure (right-branch mobile)))))) ; To consider submobiles in right-branch
  
(balanced? mob1) ; '((5 1) (6 2)) #f 5*1=5 not equal to 6*2=12

(define mob3 ;  '((5 2) (2 5))
  (make-mobile (make-branch 5 2)
	       (make-branch 2 5)))

(balanced? mob3) ; #t, 2*5=5*2=10

(define mob4 ;  '((5 2) (2  ((5 2) (2 5))))
  (make-mobile (make-branch 5 2)
	       (make-branch 2 mob3)))

(balanced? mob4) ; #f. mob3 is balanced, with weight 7, so it would be comparing: 5*2=10 with 2*7=14

; To check, if the weight of mob3 were 5, it would be balanced

(define mob3v2 ;  '((5 2) (2 4))
  (make-mobile (make-branch 5 2)
	       (make-branch 2 5)))

(define mob5 ;
  (make-mobile (make-branch 5 2)
	       (make-branch 2 mob3v2)))

(balanced? mob5) ; #f, still not, but not because the first condition, because of last one, as the submodule mob3v2 is unbalaced

(define mob3v3 ;  '((5 3) (3 5))
  (make-mobile (make-branch 5 3)
	       (make-branch 3 5)))

(define mob5v2 ;
  (make-mobile (make-branch 4 4)
	       (make-branch 2 mob3v3)))

(balanced? mob5v2) ; #t

; D

;(define (make-mobile left right)
;  (cons left right)) ; Instead of (list left right)
;
;(define (make-branch length structure)
;  (cons length structure)) ; Instead of (list length structure)

; From Exercise 2.26, the difference is that it is not needed an extra car in right-branch and branch-structure

; (define (right-branch mobile) (cdr mobile))

; (define (branch-structure branch) (cdr branch))

; Checked for sections B and C






