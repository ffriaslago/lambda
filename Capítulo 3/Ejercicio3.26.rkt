#lang sicp

;
;->Auxiliary for trees
;

(define (entry tree) (car tree)) 
(define (left-branch tree) (cadr tree)) 
(define (right-branch tree) (caddr tree)) 
  
(define (make-tree entry left right) 
  (list entry left right))

;
;->Exercise 3.26
; For this exercise it has to be assumed that keys can be ordered in some way, like numerically or alphabetically
;

;
;->Lookup procedure
;
; Now, key is just one symbol, as now the table is constructed as a binary tree. Each entry of the tree-table is a pair (key, value)
; Code inspired by the one from Exercise 2.66
; Here, tree-table is structured as a binary tree, ordered by the numerical values of the keys
;

(define (lookup key tree-table) 
  (cond ((null? tree-table) false)        
        ((equal? key (car (entry tree-table))) (cdr (entry tree-table)))
        ((< key (car (entry tree-table))) (lookup key (left-branch tree-table)))         
        ((> key (car (entry tree-table))) (lookup key (right-branch tree-table)))))

;
;->Insert procedure
;
;
;->This was a first attempt
;
;(define (insert! key value tree-table)
;  (cond   
;    ((null? tree-table) (make-tree (cons key value) '() '()))    
;    ((equal? key (car (entry tree-table))) (set-cdr! (entry tree-table) value)) ; If it is found a same key, the value is updated    
;    ((< key (car (entry tree-table)))    
;     (let ((left (left-branch tree-table)))           
;       ;       (set! tree-table (make-tree (entry tree-table)
;       ;                                   (insert! key value left)
;       ;                                   (right-branch tree-table))))) ; Go to left branch
;       (set-car! (cdr tree-table) (insert! key value left)))) ; Go to left branch   
;    ((> key (car (entry tree-table)))     
;     (let ((right (right-branch tree-table)))              
;       ;(set! tree-table (make-tree (entry tree-table)
;       ;                            (left-branch tree-table)
;       ;                            (insert! key value right))))))
;       (set-cdr! (cdr tree-table) (insert! key value right)))))
;  tree-table) ; Go to right branch
;
;->This was a second attempt
;
;(define (insertt key value tree-table)
;    (cond
;      ((null? tree-table) (make-tree (cons key value) '() '()))
;      ((equal? key (car (entry tree-table))) (set-cdr! (entry tree-table) value)) 
;      ((< key (car (entry tree-table))) 
;       (make-tree (entry tree-table) 
;                  (insert key value (left-branch tree-table)) 
;                  (right-branch tree-table))) 
;      ((> key (car (entry tree-table))) 
;       (make-tree (entry tree-table) 
;                  (left-branch tree-table) 
;                  (insert key value (right-branch tree-table))))))
;
;(define (insert key value tree-table)
;  
;  (set! tree-table (insertt key value tree-table))) 
;
;->These are different tests to check whether attempts 1 or 2 worked or not
;
;(define tree-table1 '())
;tree-table1 ; (() () ())
;(set! tree-table1 (insertt 1 'a tree-table1))
;(set! tree-table1 (insertt 2 'b tree-table1))
;(insert 1 'a tree-table1)
;tree-table1 ; ((1 . a) () ())
;(insert 1 'b tree-table1)
;(insert! 2 'b tree-table1)
;(null? tree-table1) ; #f
;(null? (entry tree-table1)) ; #f
;(equal? 2 (car (entry tree-table1))) ; #f
;(< 2 (car (entry tree-table1))) ; #f
;(> 2 (car (entry tree-table1))); #t
;(define right (right-branch tree-table1))
;right
;(set! tree-table1 (make-tree (entry tree-table1)
;                             (left-branch tree-table1)
;                             (insert! 2 'b right)))

;(set! tree-table1 (make-tree (entry tree-table1)
;                             (left-branch tree-table1)
;                             '((2 . b) () ())))


;
;->After all this work, the conclusion is clear. It is very difficult to create a procedure that takes a table in the form of a binary tree, implements the insertion and
; returns the original tree with the element inserted so it can be added more elements.
; I thought that using set! everything would be perfect, but it gives so many problems working with lists (as it is defined, make-tree is a list)
; The best is to do it ussing message-passing, so the object is ok under transformations
;


(define (make-table)
  
  (let ((local-tree '()))
      
    (define (lookup key tree-table) ; Same as above
      (cond ((null? tree-table) false) 
            ((equal? key (car (entry tree-table))) (cdr (entry tree-table))) 
            ((< key (car (entry tree-table))) (lookup key (left-branch tree-table))) 
            ((> key (car (entry tree-table))) (lookup key (right-branch tree-table)))))
    
    (define (insert key value tree-table)
      (cond
        ((null? tree-table) (make-tree (cons key value) '() '()))
        ((equal? key (car (entry tree-table))) (set-cdr! (entry tree-table) value)) 
        ((< key (car (entry tree-table))) 
         (make-tree (entry tree-table) 
                    (insert key value (left-branch tree-table)) 
                    (right-branch tree-table))) 
        ((> key (car (entry tree-table))) 
         (make-tree (entry tree-table) 
                    (left-branch tree-table) 
                    (insert key value (right-branch tree-table))))))
      
    (define (insert! key value)
      (set! local-tree (insert key value local-tree)))

    (define (print)
      (if (null? local-tree)
          '()
          (begin
            (print (left-branch local-tree))
            (display (car (entry local-tree)))
            (display ":")
            (display (cdr (entry local-tree)))
            (newline)
            (print (right-branch local-tree)))))
      
    (define (dispatch m) 
      (cond ((eq? m 'insert) insert!)
            ((eq? m 'lookup) insert!)
            ((eq? m 'print-tree) local-tree)
            ((eq? m 'print-leaves) print)
            (else (error "Undefined operation -- TABLE" m)))) 
    dispatch))

(define table-tree1 (make-table))

((table-tree1 'insert) 1 'a)

(table-tree1 'print-tree)

((table-tree1 'insert) 0 'b)

(table-tree1 'print-tree)

((table-tree1 'insert) 2 'c)

(table-tree1 'print-tree)