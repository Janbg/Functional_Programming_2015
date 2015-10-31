#lang racket

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


(define (is-prime? n)
  (loop? n (- n 1)))

