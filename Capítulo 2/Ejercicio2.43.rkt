#lang racket

; Louis solution

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

; Original code

(flatmap
 (lambda (rest-of-queens)
   (map (lambda (new-row)
          (adjoin-position new-row k rest-of-queens))
        (enumerate-interval 1 board-size)))
 (queen-cols (- k 1)))