#lang racket

(define (lookup-tree given-key tree-of-records)
  
  (cond ((null? tree-of-records) false)
        
        ((equal? given-key (car tree-of-records)) (car tree-of-records))

        ((< given-key (car tree-of-records)) (lookup-tree given-key (left-branch tree-of-records)))
         
        (else (lookup given-key (right-branch tree-of-records))))) ; else is the case (> given-key (key (car set-of-records)))