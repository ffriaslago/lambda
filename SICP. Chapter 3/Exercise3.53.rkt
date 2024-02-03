#lang sicp

(define s (cons-stream 1 (add-streams s s)))

; (add-streams s s) is equivalent to (stream-map + s s) using the 3.50 stream-map definition

; Stream 1 2 4 8 16 32 ... powers of 2

; 1 is present because it is defined like that, then, the second element would be adding the first element of s and the first element of s, so 1+1=2
; The third element would be the result of adding the second element of s to itself, and so on

