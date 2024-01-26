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
      (let ((leaf2 (car set-of-leaves)) ; it is ordered in increasing order of weights
            (leaf1 (cadr set-of-leaves)))
    
        (let ((newtree (make-code-tree leaf1 leaf2))) ; the two smalles-weight elements are merged

          (successive-merge (adjoin-set newtree (cddr set-of-leaves))))))) ; as adjoin-set compares weight, it will be integrated in the correct order 

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define counter 0)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (set! counter (+ counter 1))
  (if (not (leaf? tree))
      (let ((left-symbols (symbols (left-branch tree)))
            (right-symbols (symbols (right-branch tree))))
        (cond ((element-of-set? symbol left-symbols) (append (list 0) (encode-symbol symbol (left-branch tree))))
              ((element-of-set? symbol right-symbols) (append (list 1) (encode-symbol symbol (right-branch tree))))
              (else (error "Symbol is not in the tree at all" symbol))))              
      '()))

(define (element-of-set? x set)
  (set! counter (+ counter 1))
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; Order of growth in the number of steps to encode a symbol

(define t5 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
(define t10 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512))))
(define t20 (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)
                                           (K 1024) (L 2048) (M 4096) (N 8192) (O 16384) (P 32768) (Q 65536) (R 131072) (S 262144) (T 524288))))

(set! counter 0)
(encode-symbol 'A t5)
counter ; 23

(set! counter 0)
(encode-symbol 'A t10)
counter ; 73

(set! counter 0)
(encode-symbol 'A t20)
counter ; 248

; Least frequent O(n^2)

(set! counter 0)
(encode-symbol 'E t5)
counter ; 3

(set! counter 0)
(encode-symbol 'J t10)
counter ; 3

(set! counter 0)
(encode-symbol 'T t20)
counter ; 3

; Most frequent O(1)