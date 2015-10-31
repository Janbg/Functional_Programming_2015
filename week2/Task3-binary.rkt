#lang racket
;---Helper to take the n letter---
(define (take-letter str n)
    (~a (string-ref str n)))

;;-----------------------String Reverse:-------------------------;;
(define (string-reverse str)
  (define (str-iter i)
    (if (< i 1)
        [take-letter str 0]
        [string-append (take-letter str i) (str-iter (- i 1))]
    )
  )

  (str-iter (- (string-length str) 1))
)
;;--------------------------------------------------------------;;



;;----------------------To Binary String:-----------------------;;

;;---Helper to reverse the remainders---
(define (to-binary-string n)
  (string-reverse (consecutive-remainders n)))


(define (consecutive-remainders n)
  (if (<= n 0)
      "0"
      
      (if (<= n 1)
          "1"
          [string-append
              (~a (remainder n 2))
              (consecutive-remainders (quotient n 2))
          ]
      )
  )
)
;;--------------------------------------------------------------;;


;;--------------------From Binary String:-----------------------;;
(define (from-binary-string binary-str)
  (define (help-iter number i sum)
    (if (= i (string-length binary-str) )
        
        [+ sum (* number (expt 2 i) ) ]
        
        [help-iter (quotient number 10)
                   (+ i 1)
                   (+ sum (* (remainder number 10) (expt 2 i)))
        ]
    )
  )

(help-iter (string->number binary-str) 0 0)
)
;;--------------------------------------------------------------;;







