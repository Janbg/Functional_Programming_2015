#lang racket

;  Description:
; I make first the function "string-repeat-display"
; and I think that's not the specific idea.
; Because I print the characters :)
; Below is the 'real' function "string-repeat" :) 


;;-----------String Repeat with display:------------;;
(define (string-repeat-display str n)
  (define (str-iter i)
    (cond
      [(> i n) (display "\"")]
      [(= i 0)
       {begin
         (display "\"")
         (str-iter (+ i 1))
       }
      ]
      [else
       {begin
         (display str)
         (str-iter (+ i 1))}]
    )
  )

  (str-iter 0)
)
;---------------------------------------------------



;;------------------String Repeat:-----------------;;
(define (string-repeat str n)
  (if (< n 2)
      (string-append str)
      (string-append str (string-repeat str (- n 1)))
   )
)
;---------------------------------------------------



;;---------------------Fence:----------------------;;
;-Helper-
(define (count n)
  (round (+ 1 (log n)))
)

(define (fence n)
  (string-append "{"
                 (string-repeat "-" (count n))
                 ">"
                 (number->string n)
                 "<"
                 (string-repeat "-" (count n))
                 "}"
   )
)






