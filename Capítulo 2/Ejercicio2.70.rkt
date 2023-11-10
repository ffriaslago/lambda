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
      (let ((leaf1 (car set-of-leaves))
            (leaf2 (cadr set-of-leaves)))    
        (let ((newtree (make-code-tree leaf1 leaf2))) 
          (successive-merge (adjoin-set newtree (cddr set-of-leaves)))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define tree270 
'((leaf NA 16)
  
  ((leaf YIP 9)
   
   (((leaf A 2)
     
     ((leaf WAH 1) (leaf BOOM 1)
                   (WAH BOOM) 2)
     
     (A WAH BOOM) 4)

    ((leaf SHA 3)

     ((leaf JOB 2) (leaf GET 2)                                                                                                      
                   (JOB GET) 4)
                                                                                        
     (SHA JOB GET) 7)
                                                                          
    (A WAH BOOM SHA JOB GET) 11)
   
   (YIP A WAH BOOM SHA JOB GET) 20)
  
  (NA YIP A WAH BOOM SHA JOB GET) 36))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (not (leaf? tree))
      (let ((left-symbols (symbols (left-branch tree)))
            (right-symbols (symbols (right-branch tree))))
        (cond ((element-of-set? symbol left-symbols) (append (list 0) (encode-symbol symbol (left-branch tree))))
              ((element-of-set? symbol right-symbols) (append (list 1) (encode-symbol symbol (right-branch tree))))
              (else (error "Symbol is not in the tree at all" symbol))))              
      '())
  )

(define mes270
  '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(encode mes270 tree270)

; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(length (encode mes270 tree270)) ; 84 bits

; Fixed length code for 8-symbol alphabet means 3 bits per symbol

(length mes270) ; 36

; So 36x3= 108 bits
