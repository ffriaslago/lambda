#lang sicp

; There are several ways to approach this exercise.

; One new syntax in which eval and apply are not changed could be the one where arithmetic operations (and general functions) are written in a way
; where the operator is the last element of the list, not the first one, i.e., (1 2 3 +)
; All the procedures such as tagged-list or if? should be changed, but neither eval nor apply