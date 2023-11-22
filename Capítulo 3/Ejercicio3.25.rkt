#lang sicp

; Tables

(define (make-table)
  (list '*table*))

(define (assoc key records)
  (cond ((null? records) false) ; If no records, key will not match anything
        ((equal? key (caar records)) ; Key of the first pair of records.
         (car records))
        (else (assoc key (cdr records))))) ; It is continued with all the pairs of records except the first one

; 1-D Tables

(define (lookup1D key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert1D! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) 
                        (cdr table)))))
  'ok)

;(define table1D (make-table))
;
;(insert1D! 'a 1 table1D) ; ok
;
;(insert1D! 'b 2 table1D) ; ok
;
;(insert1D! 'c 3 table1D) ; ok 
;
;table1D ; (*table* (c . 3) (b . 2) (a . 1))
;
;(lookup1D 'b table1D) ; 2

; 2-D tables

(define (lookup2D key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (cdr subtable))))
          (if record (cdr record) false))
        false)))

(define (insert2D! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record 
               (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! 
               subtable
               (cons (cons key-2 value)
                     (cdr subtable)))))
        (set-cdr! 
         table
         (cons (list key-1 (cons key-2 value))
               (cdr table)))))
  'ok)

;(define table2D (make-table))
;
;(insert2D! 'letter 'a 1 table2D) ; ok
;
;(insert2D! 'letter 'b 2 table2D) ; ok
;
;(insert2D! 'letter 'c 3 table2D) ; ok 
;
;table2D ; (*table* (letter (c . 3) (b . 2) (a . 1)))
;
;(lookup2D 'letter 'b table2D) ; 2
;
;(insert2D! 'word 'array 1 table2D) ; ok
;
;(insert2D! 'word 'boat 2 table2D) ; ok
;
;(insert2D! 'word 'computer 3 table2D) ; ok
;
;table2D ; (*table* (word (computer . 3) (boat . 2) (array . 1)) (letter (c . 3) (b . 2) (a . 1)))
;
;(lookup2D 'word 'boat table2D) ; 2

; General Tables

(define (insert! list-key value table)

  (define (last-pair x) (if (null? (cdr x)) x (last-pair (cdr x)))) ; From the book

  (define (concatenated x value) ; Mine
    (let ((lastt (last-pair x)))
      (if (not (pair? (cdr x)))
          (cons (car lastt) value)
          (list (car x) (concatenated (cdr x) value))))) 
  
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

(define table (make-table))

(insert! (list 'math 'operands 'additive '+) 1 table)

;table ; (*table* (math (operands (additive (+ . 1))))). It does what it is supposed to do. 

(insert! (list 'math 'functions 'continue 'identity) 1 table)

;table ; (*table* (math (functions (continue (identity . 1))) (operands (additive (+ . 1))))).  It does what it is supposed to do. 

(insert! (list 'math 'operands 'multiplicative '*) 1 table)

;table ; (*table* (math (functions (continue (identity . 1))) (operands (multiplicative (* . 1)) (additive (+ . 1))))).  It does what it is supposed to do.

(insert! (list 'math 'operands 'additive '-) 2 table)

;table ; (*table* (math (functions (continue (identity . 1))) (operands (multiplicative (* . 1)) (additive (- . 2) (+ . 1))))). It does what it is supposed to do.

(define (lookup list-key table) ; There is a list with the needed itinerary, like '(word computer) in previous table  
  (let ((subtable (assoc (car list-key) (cdr table)))) ; First level association    
    (if (and subtable (not (null? (cdr list-key)))) ; If it is succesful        
        (let ((record (assoc (cadr list-key) (cdr subtable)))) ; It is tried the second-level association          
          (if record ; If it is succesful              
              (if (not (pair? (cdr record))) ; Extra condition to check if this is the lowest level or not. If it is not a pair, it's ok!                  
                  (cdr record) ; If it is an item, ended, returned                  
                  (if (not (null? (cddr list-key))) ;If not, it is needed to continue with the next key 
                      (lookup (cddr list-key) record) 
                      false))                                        
              false))                
    false)))

(define l1 (list 'math 'operands 'additive '+))
(lookup l1 table) ; 1. Yay!

(define l2 (list 'math 'operands 'additive '-))
(lookup l2 table) ; 2. Yay!

; Auxiliary tests
;(define subtable (assoc (car l1) (cdr table)))
;subtable ; (math (functions (continue (identity . 1))) (operands (multiplicative (* . 1)) (additive (- . 2) (+ . 1))))
;(define record (assoc (cadr l1) (cdr subtable)))
;record  ; (operands (multiplicative (* . 1)) (additive (- . 2) (+ . 1)))

;(lookup (cddr l1) record) ; (additive +) in (operands (multiplicative (* . 1)) (additive (- . 2) (+ . 1)))
;(define l2 (cddr l1))

;(define subtable2 (assoc (car l2) (cdr record)))
;subtable2 ; (additive (- . 2) (+ . 1))
;(and subtable2 (not (null? (cdr l2)))) ; #t
;(define record2 (assoc (cadr l2) (cdr subtable2)))
;record2 ; (+ . 1)
;(not (pair? (cdr record2)))










  

