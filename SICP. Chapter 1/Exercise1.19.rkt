#lang racket

; Tpq(a,b)=(bq+aq+ap, bp+aq)=[a(p+q)+bq, aq+bp]
; Tpq(bq+aq+ap, bp+aq)=[(bp+aq+ap)(p+q)+(bp+aq)q, (bq+aq+ap)q+(bp+aq)p]=(bqp+bq^2+aqp+aq^2+ap^2+apq+bpq+aq^2, bq^2+aq^2+apq+bp^2+aqp)=
; =[a(qp+q^2+p^2+qp+q^2)+b(qp+q^2+qp), b(q^2+p^2)+a(q^2+pq+pq)]=[a(q^2+2pq+p^2+q^2)+b(q^2+2pq), a(q^2+2pq)+b(p^2+q^2)]
; -> p'=p^2+q^2
; -> q'=q^2+2pq
; -> a-->(+ (* a (+ (* 2 (square q)) (square p) (* 2 p q))) (* b (+ (square q) (* 2 p q))))
; -> b-->(+ (* a (+ (square q) (* 2 p q))) (* b (+ (square p) (square q))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (square x) (* x x))

(define (fib-iter a b p q count)
  (cond ((= count 0) 
         b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else 
         (fib-iter (+ (* b q) 
                      (* a q) 
                      (* a p))
                   (+ (* b p) 
                      (* a q))
                   p
                   q
                   (- count 1)))))

(fib 20)