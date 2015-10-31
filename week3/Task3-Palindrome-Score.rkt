#lang racket
;;-------------------Reverse Int Number-----------------------
(define (reverse-int n)
  (define (helper n result)
    (cond
      [(< n 10) (+ (* result 10) n)]
      [else
       (helper (quotient n 10) (+ (* result 10) (remainder n 10)))]
    )
  )
  
  (helper n 0)
)
;;---------------------------------------------------------------
;;---------------Is The Int Number Palindrome ? -----------------

(define (palindrome? n)
  (= n (reverse-int n) )
)
;;---------------------------------------------------------------

;;-----The Sum of N and the Reverse of N-------------------------

(define (sum-n-and-reverse n)
  (+ n (reverse-int n))
)
;;---------------------------------------------------------------

;;------------!!!!! P-Score Function !!!! ---------------------;;
(define (p-score n)
  (define (helper n counter)
    (cond
      [(palindrome? n) counter]
      [else
       (helper (sum-n-and-reverse n) (+ counter 1))]
     )
   )

  (helper n 1)
)












