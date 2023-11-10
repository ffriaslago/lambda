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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (successive-merge set-of-leaves)
  
  (if (= (length set-of-leaves) 1)
      (car set-of-leaves)
      (let ((leaf2 (car set-of-leaves))
            (leaf1 (cadr set-of-leaves)))    
        (let ((newtree (make-code-tree leaf1 leaf2))) 
          (successive-merge (adjoin-set newtree (cddr set-of-leaves)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)))

; '(((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31)

;                                                      (A B C D E) 31
;                                                           /\
;                                                          /  \
;                                                         /    \
;                                                        /      \
;                                                       /        \
;                                                     E 16    (A B C D) 15
;                                                                /\
;                                                               /  \
;                                                              /    \
;                                                             /      \
;                                                           D 8    (A B C) 7
;                                                                    /\
;                                                                   /  \
;                                                                  /    \
;                                                                 /      \
;                                                               C 4     (A B) 3
;                                                                        /\
;                                                                       /  \
;                                                                      /    \
;                                                                     /      \
;                                                                    B 2     A 1

(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (H 128) (I 256) (J 512)))

;'((((((((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31) (leaf F 32) (A B C D E F) 63) (leaf G 64) (A B C D E F G) 127)
;    (leaf H 128)
;    (A B C D E F G H)
;    255)
;   (leaf I 256)
;   (A B C D E F G H I)
;   511)
;  (leaf J 512)
;  (A B C D E F G H I J)
;  1023)

;                                                 (A B C D E F G H I J) 1023
;                                                           /\
;                                                          /  \
;                                                         /    \
;                                                        /      \
;                                                       /        \
;                                                    J 512 (A B C D E F G H I) 511
;                                                                /\
;                                                               /  \
;                                                              /    \
;                                                             /      \
;                                                          I 256  (A B C D E F G H) 255
;                                                                    /\
;                                                                   /  \
;                                                                  /    \
;                                                                 /      \
;                                                              H 128  (A B C D E F G) 127
;                                                                        /\
;                                                                       /  \
;                                                                      /    \
;                                                                     /      \
;                                                                   G 64  (A B C D E F) 63
;                                                                            /\
;                                                                           /  \
;                                                                          /    \
;                                                                         /      \
;                                                                       F 32  (A B C D E) 31 (From here, it is the same tree that the n=5 case

; Most frequent symbol, 1 bit, it should be 1
; Second most frequent symbol, 2 bits, 01
; Third most frequent symbol, 3 bits, 001
; Fourth most frequent symbol, 4 bits, 0001
; Fifth most frequent symbol, 5 bits, 00001
; ...
; Least frequent symbol, n-1 bits, (n-1)0s
