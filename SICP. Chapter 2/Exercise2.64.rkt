#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements) ; elements is an ordered list. It is converted into a balanced binary tree. 
  (car (partial-tree elements (length elements)))) ; first call, the integer coincides with the length of the list. It selects car!

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts) ; It creates a list with its first element '(). Then, as list->tree only returns car of this, it will return '(), the tree has only one node, being it empty
      
      (let ((left-size (quotient (- n 1) 2))) ; quotient returns an integer, the quotient of dividing by 2
        
        (let ((left-result (partial-tree elts left-size))) ; This is applied on the left side of the list, if the length is odd, not including the central element
          
          (let ((left-tree (car left-result)) ; it applies the list->tree procedure internally, it generates the left-branch
                (non-left-elts (cdr left-result)) ; as it can be seen in the final line, the cdr of applying partial-tree is the remaining elements not included in the tree
                (right-size (- n (+ left-size 1)))) ; it is the length of the remaining elements not present in left-tree
            
            (let ((this-entry (car non-left-elts)) ; First of the elements not present in left-tree. It would be the first and central node of the tree
                  (right-result (partial-tree (cdr non-left-elts) right-size))) ;  This is applied on the right side of the list, not including the central element
              
              (let ((right-tree (car right-result)) ; it applies the list-tree procedure internally, it generates the right branch
                    (remaining-elts (cdr right-result))) ; final remaining elements
                
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree) ; it generates the final tree as the first par of the return of partial-tree (because list->tree applies car)
                      remaining-elts)))))))) ; remaining elements from the initial list, second part of the return

(list->tree '(1 3 5 7 9 11)) ; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
;(car (partial-tree '(1 3 5 7 9 11) 6))
;(partial-tree '(1 3 5 7 9 11) 6)
;left-size=2
;left-result=(partial-tree '(1 3 5 7 9 11) 2)
;(partial-tree '(1 3 5 7 9 11) 2)
;left-size=0
;left-result=(partial-tree '(1 3 5 7 9 11) 0)=(cons '() '(1 3 5 7 9 11))
;left-tree='()
;non-left-elts='(1 3 5 7 9 11)
;right-size=5
;this-entry=1
;right-result=(partial-tree '(3 5 7 9 11) 5)
;(partial-tree '(3 5 7 9 11) 5)
;left-size=2
;left-result=(partial-tree '(3 5 7 9 11) 2)
...
; This procedure divides the list into parts, left, central node and right and it calls itself recursively constructing the subbranches 

;'(5               ; central node 
;                  ;
;  (1 ()           ;
;     (3 () ()))   ;
;                  ;
;  (9 (7 () ())    ;
;     (11 () ()))) ;
;
;                  5
;                 / \
;                /   \
;               /     \
;              /       \
;             1         9
;              \       / \
;               \     /   \
;                3   7    11

; Order of growth

; Master theorem (Wikipedia)

; T(n)=a*T(n/b) + f(n)
; c_crit=log_b(a)

; if f(n) has O(n^c) with c<c_crit, then T(n)=O(n^(c_crit))

; partial-tree calls itself twice with half of the length, so a=2, b=2 and c_crit=1
; the rest of instructions have O(1), so f(n)=O(1), so c=0 and c<c_crit

; From Master Theorem, then T(n)=O(n)


