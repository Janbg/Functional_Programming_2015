#lang racket

;;--Връща списък от всички елементи на list-elements,
;;  за които са изпълнени всички предикати в list-predicates

(define (where list-elements list-predicates)
  (cond
    [(empty? list-elements) '()]  
    [(empty? list-predicates) list-elements]
    [else (where (filter (first list-predicates) list-elements)
                 (rest list-predicates))]))


