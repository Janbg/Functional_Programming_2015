#lang racket

(define (circle? circle-x circle-y radius point-x point-y)
  (<=
   (+ (expt (- point-x circle-x) 2)
      (expt (- point-y circle-y) 2))
   (* radius radius)))