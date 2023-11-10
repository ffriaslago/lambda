#lang racket

; Huffman Encoding Trees

; Auxiliary procedures for making a tree

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; Constructor of the encoded tree

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; Selectors

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; We will represent a set of leaves and trees as a list of elements, arranged in increasing order of weight. 

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

; The following procedure takes a list of symbol-frequency pairs such as ((A 4) (B 2) (C 1) (D 1))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;(make-leaf-set '((A 4) (B 2) (C 1) (D 1))) ; '((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))
;(make-leaf-set '((A 4) (B 2) (C 1) (D 3))) ; '((leaf C 1) (leaf B 2) (leaf D 3) (leaf A 4))

; make-leaf-set orders the leaves in increasing order

(define (successive-merge set-of-leaves)
  
  (if (= (length set-of-leaves) 1)
      ; one leaf, the job is done
      (car set-of-leaves)
      ; two leafs or more, continue
      (let ((leaf1 (car set-of-leaves)) ; it is ordered in increasing order of weights
            (leaf2 (cadr set-of-leaves)))
    
        (let ((newtree (make-code-tree leaf1 leaf2))) ; the two smalles-weight elements are merged

          (successive-merge (adjoin-set newtree (cddr set-of-leaves))))))) ; as adjoin-set compares weight, it will be integrated in the correct order 

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))) ; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

; Too see it more easily
(define set-of-leaves (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))) 
(define leaf1 (car set-of-leaves))
(define leaf2 (cadr set-of-leaves))
(define newtree (make-code-tree leaf1 leaf2)) ; '((leaf D 1) (leaf C 1) (D C) 2)
(adjoin-set newtree (cddr set-of-leaves)) ; '((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (leaf A 4))


  
