#lang racket

(define (make-account balance password)
  (let ((counter 0))
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
    (define (call-the-cops amount)
      "POLICE IS COMING")
    (define (dispatch p m)
      (if (eq? p password)
          (begin (set! counter 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT"
                                    m))))
          (cond ((>= counter 7) call-the-cops)
                (else (set! counter (add1 counter))
                      incorrect-password))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40) ; 60

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((acc 'some-other-password 'deposit) 50) ; "POLICE IS COMING"