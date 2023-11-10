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

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

; Auxiliary procedures for encode-symbol

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;
; Exercise 2.68
;

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;(define (encode-symbol symbol tree))

; Objective (encode '(A D A B B C A) sample-tree) returns '(0 1 1 0 0 1 0 1 0 1 1 1 0)

;(car message) will be the first element of the message given, i.e., a letter
;
;The tree containing the code is here too
;
(define (encode-symbol symbol tree)
  (if (not (leaf? tree))
      ; If it is a tree, let's do this
      (let ((left-symbols (symbols (left-branch tree)))
            (right-symbols (symbols (right-branch tree))))
        (cond ((element-of-set? symbol left-symbols) (append (list 0) (encode-symbol symbol (left-branch tree))))
              ((element-of-set? symbol right-symbols) (append (list 1) (encode-symbol symbol (right-branch tree))))
              (else (error "Symbol is not in the tree at all" symbol))))              
      ; If it is a leave, let's do this 
      '())
  )

(encode '(A D A B B C A) sample-tree) ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)




