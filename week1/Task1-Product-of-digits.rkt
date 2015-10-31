#lang racket

(define (last-digit n)
  (remainder n 10))

(define (remove-last-digit n)
  (quotient n 10))

(define (product-digits n)
  (if (< n 10)
      n
      (* (last-digit n)
         (product-digits (remove-last-digit n)))))