#lang racket
;-----------------Series-------------------
(define (series a b n)
  (define (series-iter a b i)
    (if (> i n)
        b
        (series-iter b (+ a b) (+ i 1))))

  (series-iter a b 3)
)
;------------------------------------------

;---Lucas:
(define (lucas n)
  (if (= n 1)
      2
      (series 2 1 n))
)

;---Fibonacci
(define (fibonacci n)
  (series 1 1 n)
)

;---Summed Member
(define (summed-member n)
  (+ (lucas n) (fibonacci n))
)

;------Nth Function Sum (for Lucas and Fibonacci)------
(define (nth-function-sum n NAME)
  (define (function-iter i result)
  (if (>= i n)
      (+ result (NAME i))
      (function-iter (+ i 1) (+ result (NAME i)))))

  (function-iter 1 0)
)
;------------------------------------------------------

;---Nth Lucas Sum
(define (nth-lucas-sum n)
  (nth-function-sum n lucas)
)

;---Nth Fibonacci Sum
(define (nth-fibonacci-sum n)
  (nth-function-sum n fibonacci)
)

;---The Difference Series
(define (lucas-fib-diff n)
  (- (lucas n) (fibonacci n))
)


