#lang racket

;Auxiliary procedures for binary trees implementation
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; Procedure 1

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

; Procedure 2

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; After a first look, the result should be the same. It generates the same list for the three trees.

(define t1 (make-tree 7
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 (make-tree 5 '() '()))
                      (make-tree 9
                                 '()
                                 (make-tree 11 '() '()))))
;
; Section A
;

(tree->list-1 t1) ; '(1 3 5 7 9 11)
(tree->list-2 t1) ; '(1 3 5 7 9 11)

(tree->list-1 (make-tree 1 '() '())) ; '(1)
(tree->list-2 (make-tree 1 '() '())) ; '(1)

;(tree->list-1 t1)
;(append (tree->list-1 (make-tree 3
;                                 (make-tree 1 '() '())
;                                 (make-tree 5 '() '())))
;        (cons 7
;              (tree->list-1 (make-tree 9
;                                       '()
;                                       (make-tree 11 '() '())))))
;; Tree by tree
;(tree->list-1 (make-tree 9
;                         '()
;                         (make-tree 11 '() '())))
;(append '()
;        (cons 9
;              (tree->list-1 (make-tree 11
;                                       '()
;                                       '()))))
;(append '()
;        (cons 9 (append '() (cons 11 '())))) ; '(9 11)
;(tree->list-1 (make-tree 3
;                         (make-tree 1 '() '())
;                         (make-tree 5 '() '())))
;(append '(1)
;        (cons 3 '(5))) ; '(1 3 5)
;; Result
;(append '(1 3 5)
;        (cons 7 '(9 11))) '(1 3 5 7 9 11)

;(tree->list-2 t1)
;(copy-to-list t1 '())
;(copy-to-list (make-tree 3
;                         (make-tree 1 '() '())
;                         (make-tree 5 '() '()))
;              (cons 7
;                    (copy-to-list (make-tree 9
;                                             '()
;                                             (make-tree 11 '() '()))
;                                  '())))
;; Tree by tree
;(copy-to-list (make-tree 9
;                         '()
;                         (make-tree 11 '() '()))
;              '())
;(copy-to-list '()
;              (cons 9
;                    '(11))) ; '(9 11)
;; This gives
;(copy-to-list (make-tree 3
;                         (make-tree 1 '() '())
;                         (make-tree 5 '() '()))
;              '(7 9 11)) ; '(1 3 5 7 9 11)

; By construction, I don't see any reason to have different results for the same tree. Both reconstruct first the right branch, add the entry with cons and after that the left branch
; to the left. The method is quite different, but after using it with three different trees (not of all them balanced) and the deconstruction of both of them, I would say they return
; the same list to every tree

(define t2 (make-tree 3
                      (make-tree 1 '() '())
                      (make-tree 7
                                 (make-tree 5 '() '())
                                 (make-tree 9
                                            '()
                                            (make-tree 11 '() '())))))

(tree->list-1 t2) ; '(1 3 5 7 9 11)
(tree->list-2 t2) ; '(1 3 5 7 9 11)

(define t3 (make-tree 5
                      (make-tree 3
                                 (make-tree 1 '() '())
                                 '())
                      (make-tree 9
                                 (make-tree 7 '() '())
                                 (make-tree 11 '() '()))))

(tree->list-1 t3) ; '(1 3 5 7 9 11)
(tree->list-2 t3) ; '(1 3 5 7 9 11)

;
; Section B
;

; Both of the procedures apply themselves to the left branch and right branch of the tree
; Assuming a tree with n nodes takes T(n) time, both of them have 2*T(n/2) instructions for left and right branch
; So, the problem reduces to see how this branches are added

; tree->list-1 uses append and cons
; tree->list-2 uses only cons

; Intuitively one would say that, then, both procedures take a different time and the second procedure is faster

; Let's see

;(tree->list-1 (make-tree 1
;                         '()
;                         '()))
; Here you have to do
;(append '() (cons 1 '())) ; It uses append all the time, one time for each node

;(tree->list-2 (make-tree 1
;                         '()
;                         '()))
; Here you have to do
;(copy-to-list (make-tree 1
;                         '()
;                         '())
;              '())
;(copy-to list '()
;         (cons 1
;               (copy-to-list '() '())))
;(cons 1
;      (copy-to-list '() '()))
;(cons 1 '()) ; It just uses cons one time for each node

; From the definition of append, it uses a recursive plan, so its growth is O(n)
; cons has growth O(1)

         



