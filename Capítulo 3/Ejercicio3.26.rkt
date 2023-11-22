#lang sicp

; Auxiliary for trees

(define (entry tree) (car tree)) 
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
  
(define (make-tree entry left right) 
  (list entry left right))

; For this exercise it has to be assumed that keys can be ordered in some way, like numerically or alphabetically

; Lookup procedure

; Now, key is just one symbol, as now the table is constructed as a binary tree. Each entry of the tree-table is a pair (key, value)
; Code inspired by the one from Exercise 2.66
; Here, tree-table is structured as a binary tree, ordered by the numerical values of the keys 

(define (lookup key tree-table) 
  (cond ((null? tree-table) false)        
        ((equal? key (car (entry tree-table))) (cdr (entry tree-table)))
        ((< key (car (entry tree-table))) (lookup key (left-branch tree-table)))         
        ((> key (car (entry tree-table))) (lookup key (right-branch tree-table)))))

; Insert procedure (pending)

; From book, for binary-trees, it will be needed

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (insert! key value tree-table)
  
  (let ((subtable (assoc (car list-key) (cdr table))))                ; (0)-start    
    (if (and subtable (not (null? (cadr list-key))))                  ; (1)-start        
        (let ((record (assoc (cadr list-key) (cdr subtable))))        ; (2)-start   
          (if (and record (not (null? (cddr list-key))))              ; (3)-start
              (if (not (pair? (cdr record)))                          ; (4)-start
                  (set-cdr! record value)
                  (insert! (cddr list-key) value record))               ; (4)-end
              ; From here, it works 
              (insert! (cdr list-key) value subtable)))                ; (3)-end, (2)-end. ; If there is not this subtable, it is needed to construct it                                                                               
        (set-cdr! table
                  (cons (concatenated list-key value)
                        (cdr table)))))                                     ; (1)-end, (0)-end
  'ok)
