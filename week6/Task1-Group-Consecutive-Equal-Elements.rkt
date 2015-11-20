#lang racket

;;#######=======-------> Groups Consecutive Equal Elements Into Sub-lists <-------=======#######;;
(define (group XS)
  (define (help view temp result)
    (cond
      [(= (length XS) 0) '() ]
      [(empty? view) (reverse (cons temp result))]
      [(or (empty? temp) (= (first view) (first temp)))
                                                        (help (rest view) (cons (first view) temp) result)]
     
      [else (help (rest view) (list (first view)) (cons temp result))]    ;;(not (= (first view) (first temp)))
    )
  )

  (help XS '() '())
)
