#lang racket

(define (p a b c)
  (/
   (+ a b c)
   2))

(define (area a b c)
  (sqrt
   (* (p a b c)
      (- (p a b c) a)
      (- (p a b c) b)
      (- (p a b c) c))))