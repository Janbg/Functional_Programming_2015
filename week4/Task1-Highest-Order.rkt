#lang racket

;--- Main Function----;

(define (f pred g h)
  (λ (x) (and (pred (g x)) (pred (h x)) ))
)



;--Prime Function---;

(define (prime? n)
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
;;----------------------------------------------------;;



;;--Square Function---;

(define (square x)
  (* x x)
)
;;----------------------------------------------------;;
