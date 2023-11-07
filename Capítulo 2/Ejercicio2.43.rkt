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

; Assuming board-size=8
; The difference is swaping (queen-cols (- k 1)) with (enumerate-interval 1 8)
; With the original code, (queen-cols (- k 1)) is called one time for each k.
; So, if we start with (queen-cols 8), this means we have to calculate (queen-cols 7) once, which means that we have to calculate
; (queen-cols 6) once and so on
; With Louis solution, more times, (queen cols (- k 1)) has to be reevaluated one time for every item in (enumerate-interval 1 8), i.e., 8 times
; So, if we start with (queen-cols 8), this means we have to calculate (queen-cols 7) 8 times, which means that we have to calculate
; (queen-cols 6) 64 times and so on
;(queen-cols 8)->1
;(queen-cols 7)->8
;(queen-cols 6)->64, 8^2
;(queen-cols 5)->512, 8^3
;(queen-cols 4)->8^4
;(queen-cols 3)->8^5
;(queen-cols 2)->8^6
;(queen-cols 1)->8^7
;(queen-cols 0)->8^8
; One good estimation is that if original code took T time, Louis code would take 8^8*T time for the 8x8 board