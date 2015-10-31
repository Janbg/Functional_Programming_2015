#lang racket
;   Description of helpers:

; 1. (to-binary-string n) ---> returns the binary value from number
;                              and it returns STRING
; 2. (binary-number n) --> returns the binary value from number
;                              and it returns NUMBER
;
; 3. a) (reverse-int n) ---> returns reverse int number
;    b) (palindrome-binary? n) ---> If the INT number is Palindrome
;                                   1. First convert int --> binary
;                                   2. And then if the binary is Palindrome
;                                   --> returns #t or #f
;    c) (binary-number-has-odd-number-of-ones? n) --> returns #t or #f
;
; 4. (is-hack-number n) ---> If  3. b)
;                                and     are #t 
;                                3. c)
;



;;#########################=--.1.--=#############################;;
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
;;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>;;


;;#########################=--.2.--=#############################;;
(define (binary-number n)
  (string->number (to-binary-string n))
)
;;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>;;


;;#########################=--.3.--=#############################;;
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
;--1. First convert int number ---> to binary
;--2. And then return if the binary number is Palindrome
(define (palindrome-binary? n)
  (= (binary-number n) (reverse-int (binary-number n)))
)
;;------------------------------------------------------------------------

;;---------------Is The Binary Number has odd number of 1's (ones) ? -----
(define (binary-number-has-odd-number-of-ones? n)
  (define (helper n count-ones)
    (cond
      [(and (< n 10) (= n 1)) (odd? (+ count-ones 1))]
      [(and (< n 10) (= n 0)) (odd? count-ones) ]
      [(= (remainder n 10) 1) (helper (quotient n 10) (+ count-ones 1))]
      [else (helper (quotient n 10) count-ones)]
    )
  )

  (helper (binary-number n) 0)
)

;;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>;;

;;#########################=--.4.--=#############################;;

(define (is-hack-number n)
  (and (palindrome-binary? n) (binary-number-has-odd-number-of-ones? n)))



;;!!!! AND FINALLY THE FUNCTION !!!!!!;;

(define (next-hack n)
  (define (help n)
    (cond
      [(is-hack-number n) n]
      [else (help (+ n 1))]
     )
  )

  (help (+ n 1))
)














