#lang racket

;;------------------String Repeat:-----------------;;
(define (string-repeat str n)
  (if (< n 2)
      (string-append str)
      (string-append str (string-repeat str (- n 1)))
   )
)
;---------------------------------------------------;;


;;------------Nth Beast Number -------------;;
(define (nth-beast-number n)
  (string->number (string-repeat "666" n))
  )
;;------------------------------------------;;










