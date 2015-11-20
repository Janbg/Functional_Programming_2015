#lang racket

;;##########==========---------->  Run-length Encoding Algorithm <----------==========##########;;
(define (run-length-encode str)
  (define (help view char count result)
    (cond
      [(vacio? view) (str-append char count result)]
      [(equal? (get-first view) char) (help (remove-first view) char (+ count 1) result)]
      [else (help (remove-first view) (get-first view) 1 (str-append char count result))]  ;; (not (equal? (get-first view) char))
    )
  )

(help (remove-first str) (get-first str) 1 "")
  )



;;##########==========---------->  Run-length Decoding Algorithm <----------==========##########;;
(define (run-length-decode str)
  (define (help view result)
    (cond
      [(vacio? view) result]
      [(not (is-number? (get-first view)))
       (help (remove-first view) (string-append result (get-first view)))]
       
       [else
       (help (cut-chars view (+ (string-length (get-number view)) 1))
             (concat (get-first (cut-chars view (string-length (get-number view))))
                      (string->number (get-number view))
                      result) )]
     )
  )
  
  (help str "")
)






;;-----------Helpers-------------;;


;;---Reverse String:
(define (rev-string str)
  (list->string (reverse (string->list str)))
)

;;---Remove First Char From String:
(define (remove-first str)
  (cond
    [(vacio? str) ""]
    [else (rev-string (~a (rev-string str) #:max-width (- (string-length str) 1)))])
)

;;---Get First Char from String:
(define (get-first str)
  (~a str #:max-width 1)
)

;;---Return if String is Empty:
(define (vacio? str)
  (equal? str ""))


;;---Similar to string-append:
(define (str-append char count result)
  (cond
    [(= count 1) (string-append result char)]
    [else (string-append result (number->string count) char)]
  )
)


;;---Similar to concatenate:
(define (concat char count result)
  (cond
    [(> count 0) (concat char (- count 1) (string-append result char))]
    [else result]))


;;---Number or not:
(define (is-number? char)
  (if (equal? (string->number char) #f)
      #f
      #t))


;;---Get Number of string:  
(define (get-number str)
  (define (help view result)
    (if (is-number? (get-first view) )
      (help (remove-first view) (string-append result (get-first view)))
      result
    )
  )

(help str "")
  )


;;---Cut first k symbols of string:
(define (cut-chars str k)
    (cond
      [(<= k 0) str]
      [else (cut-chars (remove-first str) (- k 1))]
     ) 
)


