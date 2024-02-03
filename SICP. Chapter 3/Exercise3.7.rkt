#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        incorrect-password))
  dispatch)

(define (make-joint account acc-password new-password)
  (define (incorrect-password amount) ; For the dispatch
    "Incorrect password") 
  (define (dispatch p m)
          (if (eq? p new-password) ; If the joint account is accessed correctly
              (cond ((eq? m 'withdraw) (account acc-password 'withdraw)) ; Call to original account
                    ((eq? m 'deposit) (account acc-password 'deposit)) ; Call to original account
                    (else (error "Unknown request: MAKE-ACCOUNT"
                                 m)))
              incorrect-password))
  (if (eq? ((account acc-password 'deposit) 0) '"Incorrect password") ; If the original password is not matched
      (error "Original account password not correct")              
        dispatch)) ; If it is correct, dispatch is called

(define peter-acc (make-account 100 'open-sesame))

; Original account, peter-acc, with a password and an initial balance of 100

(define paul-acc (make-joint peter-acc 
                             'open-sesame
                             'rosebud))

; Creation of joint accound, with password 'rosebud

((paul-acc 'rosebud 'withdraw) 20) ; 80 ! It works

