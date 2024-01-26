#lang racket

;
; Section A
;

; Assuming that there already exists a get-record method for each division.
; So there exists a table indexed by division name and the operation procedure.
; Each of those procedures has one argument, an identification for the employee

(define (get-record division employee-id)
  ((get division 'record) employee-id))

;
; Section B
;

; Same reasoning as in Section A, but now the get-record procedure has been implemented, so it can be used its return

(define (get-salary division record)
  ((get division 'salary) record))

;
; Section C
;

; Seeing the differences with respect to Section A, it should be given as an argument a list with all the division names 

(define (find-employee-record employee-id division-list) 
   (cond ((null? division-list) '())
         ((not (null? (get-record (car division-list) employee-name))) (get-record (car division-list) employee-name))
         (else (find-employee-record employee-name (cdr division-list)))))

;
; Section D
;

; This new company needs a new different label for each division and then implement the procedures get-record and get-salary and
; put them in the indexed table. The methods in A,B and C should work then.
