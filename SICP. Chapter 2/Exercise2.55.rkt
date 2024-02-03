#lang racket

(car ''abracadabra) ; 'quote

; From the book, footnote 34, (quote (a b c))='(a b c)

; (car ''abracadabra) can be rewritten as (car (quote (quote abracadabra)))

(car (quote (quote abracadabra)))

; first quote is understood as a procedure, which gives

(car '(quote abracadabra))

; '(quote abracadabra) is a list of two elements, being the first one 'quote, the return of applying car to it
