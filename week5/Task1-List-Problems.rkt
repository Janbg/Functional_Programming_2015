#lang racket

(define my-list (list 1 2 3 4 5))


;;---------------Намира сумата на всички числа в numbers-------------;;
;--> 1. First Way
(define (sum-lst XS)
  (apply + XS))

;--> 2. Second Way
(define (sum numbers)
  (define (help lst result)
    (cond
      [(null? lst) result]
      [else (help (cdr lst) (+ result (car lst)))]
    )
  )

(help numbers 0)
  )


;;------------Проверява дали x се среща в items---------------------;;

(define (member? x items)   ;-> Here I use the default func. 'member'
  (if (member x items)      ;-- In the default 'member' ,
      #t                    ;-- when the x exists in the list-(items)
      #f                    ;-- the func. return the tail of list starting with x
   )
)


;;------------ Намира най-големия/малкия елемент в списък ---------------------;;
;--> 1. max
(define (maximum l) (foldr max (car l) l))

;--> 2. min
(define (minimum l) (foldr min (car l) l))


;;------------Find the range from a to b  [a , b]---------------------;;
;--> 1. First Way
(define (range2 a b)
  (define (help a lst)
    (cond
      [(>= a b) lst]
      [else (help (+ a 1) (cons a lst))]
    )
  )

(reverse (help a '()))
  )

;--> 2. Second Way
(define (range3 a b)
  (cond
    [(>= a b) (list)]
    [else (cons a (range3 (+ a 1) b))]
  )
)



;;-----------------Find the length of list---------------------;;
(define (length3 items)
  (cond
    [(null? items) 0]
    [else (+ 1 (length3 (cdr items)))]
  )
)




;;-----Връща n-тия елемент от items при 0-лево базиран индекс--------;;
;--> 1. First Way
(define (list-ref2 items n)
  (define (help lst count)
    (cond
      [(= n count) (car lst)]
      [else (help (cdr lst) (+ count 1))]
    )
  )

  (help items 0)
)


;--> 2. Second Way
(define (list-ref3 items n)
  (cond
    [(zero? n) (car items)]
    [else (list-ref3 (cdr items) (- n 1))]
  )
)




;;----------Строи списък от числата между 0 и n, включително------------;;
;; като прилага f върху всяко число
;; i-тия елемент на този списък е (f i)

(define (build-list2 n f)
  (define (help count lst)
    (cond
      [(> count n) (error "Error with n \n expected: exact-nonnegative-integer")]
      [(= count n) (reverse lst)]  
      [else (help (+ count 1) (cons
                                    (f count)
                                    lst))]
    )
  )

(help 0 '())
  )




;;-----------Конкатенира два списъка в нов списък---------------------;;

(define (append2 l1 l2)
  (define (help view lst flag)
    (cond
      [(and (not flag) (null? view)) (reverse lst)] 
      [(and flag (null? view)) (help l2 lst #f)]
      [else (help (cdr view) (cons
                                   (car view)
                                   lst)
                                              flag)]
    )
   )

(help l1 '() #t)
  )




;;-----------------Обръща списъка наобратно----------------------------;;

(define (reverse2 items)
  (define (help view result)
    (cond
      [(null? view) result]
      [(help (cdr view) (cons (car view) result))]
    )
  )

(help items '())
  )




;;-------------Взима първите n елемента от даден списък-------------------;;

(define (take2 n items)
  (define (help view count result)
    (cond
      [(< count n) items]
      [(> count n) (error "Error with n \n expected: exact-nonnegative-integer")]
      [(= count n) (reverse result)]
      [(help (cdr view) (+ count 1) (cons
                                          (car view)
                                           result))]
    )
  )

(help items 0 '())
  )




;;---------------Маха първите n елемента от даден списък-------------------;;

(define (drop2 n items)
  (define (help view count result)
    (cond
      [(< (length items) n) '()]
      [(> count (- (length items) n)) (error "Error with n \n expected: exact-nonnegative-integer")]
      [(= count (- (length items) n)) result]
      [(help (cdr view) (+ count 1) (cons
                                          (car view)
                                           result))]))

  
(help (reverse items) 0 '())
  )




;;----------------------Функция от по-висок ред-------------------------;;
;; Взима поредни елементи от items докато предиката p за тях дава истина

(define (take-while pre items)
  (define (help view result)
    (cond
      [(not (pre (car view))) (reverse result)]
      [else (help (cdr view) (cons (car view) result))]
    )
  )

(help items '())
  )



;;----------------------Функция от по-висок ред-------------------------;;
;; Маха поредните елементи от items докато предикатa p дава лъжа за тях

(define (drop-while pre items)
  (define (help view count)
    (cond
      [(not (pre (car view))) (drop2 count items)]
      [else (help (cdr view) (+ count 1))]
    )
  )

  (help items 0)
)




;;----------Функцията взима число и връща списък от цифрите му------------;;

(define (number->list n)
  (define (help number result)
    (cond
      [(= (quotient number 10) 0) (cons (remainder number 10) result)]
      [(help (quotient number 10) (cons
                                       (remainder number 10)
                                        result))]
    )
  )

(help n '())
  )




;;----------Функцията взима списък от цифри и връща числото--------------;;

(define (list->number ns)
  (define (help view result)
    (cond
      [(null? view) result]
      [(help (cdr view) (+ (* result 10) (car view)))]
    )
  )

(help ns 0)
)



