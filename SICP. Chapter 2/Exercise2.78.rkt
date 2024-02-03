#lang racket

; Goal: The system should work as before except that ordinary numbers should be represented simply as Scheme numbers rather than as a pair whose car is the symbol scheme-number

; From the installation of scheme-number-package: it is used attach-tag to attach the tag 'scheme-number. If this is not wanted and attach-tag has to be changed:

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

; For type-tag. Now the scheme-numbers don't have a tag attached to them, they are just numbers. As it exists the primitive procedure number? to detect these numbers, if this is
; the case, the tag will be set manually

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

; Same reasoning for this adaptation, if datum is a number, it doesn't have a tag

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))