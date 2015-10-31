#lang racket

(define (sum a b)
  (+ (expt a 3) (expt b 3)))

(define (loop n a b)
  (if (= (sum a b) n)
      #t
      (if (and (> (sum a b) n) (equal? a b))
          #f
          (if (< (sum a b) n)
              (loop n (+ a 1) b)
              (loop n (+ b 1) (+ b 1))))))

; I put a>=0 and b>=0 
; Therefore  8 = 2^3 + 0^3  is TRUE
; In the example is not specified value for 'a' and 'b'
; You can change the starting values from this function
; Example: (loop n 1 1)  ; a>=1 and b>=1

(define (funk from to counter)
  (if (> from to)
      counter
      (if (loop from 0 0)  ; Here is the replacement
          (funk (+ from 1) to (+ counter 1))
          (funk (+ from 1) to counter))))

(define (count-cube-sums from to)
  (funk from to 0))