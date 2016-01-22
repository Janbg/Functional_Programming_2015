#lang racket

(define (is-prime? n)
  (define (loop? n variable)
  (if (< n 2)
      #f
      (if (or (= n 2) (= n 3) (= n 5))
          #t
          (if (= variable 2)
              (if (integer? (/ n 2))
                  #f
                  #t)
              (if (integer? (/ n variable))
                  #f
                  (loop? n (- variable 1)))))))
  
  (loop? n (- n 1))
)



(define (truncatable-prime? x)
  (if (< x 10)
      (is-prime? x)
      (and (is-prime? x) (truncatable-prime? (quotient x 10)))))



