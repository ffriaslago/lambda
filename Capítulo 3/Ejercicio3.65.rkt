#lang sicp

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define ln2-streamv2
  (euler-transform ln2-stream))

(define ln2-streamv3
  (accelerated-sequence euler-transform ln2-stream))