#lang racket

(define my-frac (cons 1 2))
;;-----------Helpers-----------;;
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (print-frac frac)
  (/ (fst frac) (snd frac)))
;;-----------------------------;;


;;---------------------------Simplify Fraction--------------------------------;;
(define (simplify-frac frac)
  (define (help frac divisor)
    (cond
      [(or (= (fst frac) 1) (= divisor 1)) frac]
      [(and (integer? (/ (fst frac) divisor))
            (integer? (/ (snd frac) divisor)))
       (help (cons (/ (fst frac) divisor) (/ (snd frac) divisor))
             (- divisor 1))]
      [else (help frac (- divisor 1))]
    )
  )

  (print-frac (help frac (fst frac)))
)


;;---------------------------Add Two Fractions--------------------------------;;
(define (add-frac frac1 frac2)
  (cond
    [(= (snd frac1) (snd frac2))
     (simplify-frac (cons
                          (+ (fst frac1) (fst frac2))
                          (snd frac1)))]
    [else
     (simplify-frac (cons
                          (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1)))
                          (* (snd frac1) (snd frac2))))]
  )
)



;;---------------------------Subtract Two Fractions----------------------------;;
(define (substract-frac frac1 frac2)
  (cond
    [(= (snd frac1) (snd frac2))
     (simplify-frac (cons
                          (- (fst frac1) (fst frac2))
                          (snd frac1)))]
    [else
     (simplify-frac (cons
                          (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1)))
                          (* (snd frac1) (snd frac2))))]
  )
)



;;---------------------------Multiply Two Fractions----------------------------;;
(define (mult-frac frac1 frac2)
  (simplify-frac (cons
                       (* (fst frac1) (fst frac2))
                       (* (snd frac1) (snd frac2)))
  )
)



