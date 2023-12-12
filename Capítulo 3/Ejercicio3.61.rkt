#lang sicp

(define (invert-unit-series s) ; First coefficient of s is 1
  (let ((sr (stream-cdr s)))
    (cons-stream 1
                 (scale-stream (mul-series sr (invert-unit-series s)) -1))))