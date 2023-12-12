#lang sicp

; The question is about thir procedure

(define fibs 
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs) fibs))))

; 0th element: 0 additions
; 1st element: 0 additions
; 2nd element: 1 addition
; 3rd element: 2 additions
; ...
; nth element: n additions, O(n)

; When delay is memoized, an addition can be done looking up terms that are already calculated

; If it was used the alternative expression for delay, to compute the nth term, which would be adding the (n-2)th and (n-1)th term would need to do the necessary
; additions to calculate both terms, so it would be O(phi^n), as it has been already explained in the book commenting the fibonacci algorithm