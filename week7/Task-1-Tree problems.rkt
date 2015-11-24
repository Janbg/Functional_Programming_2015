#lang racket

;;----------Helpers--------;;
(define (atom? x)
  (not (or
        (null? x)
        (pair? x)
        (vector? x))))

(define (mktree n left right)
  (list n left right))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (mkleaf n)
  (mktree n '() '() ))

(define (is-leaf? t)
  (and (empty? (left t)) (empty? (right t)) ))

(define (mkempty) '() )

(define (height-tree t)
  (if (empty? t)
      0
      (+ 1 (max (height-tree (left t))
                (height-tree (right t))))))

(define (size-tree t)
  (cond
    [(empty? t) 0]
    [(+ 1 (size-tree (left t))
          (size-tree (right t)))]))


;;-------- Example of Tree
(define t
  (mktree 1
          (mktree 2
                  (mkleaf 4)
                  (mkleaf 5))
          (mktree 3
                  (mkempty)
                  (mkleaf 6))
   )
)

;;-------- Example of Binary Search Tree
(define bst
  (mktree 8
           (mktree 3
                    (mkleaf 1)
                    (mktree 6
                             (mkleaf 4)
                             (mkleaf 7)))
           (mktree 10
                    (mkempty)
                    (mktree 14
                             (mkleaf 13)
                             (mkempty)))))


;;================>>>>>>>>>>>>>>   Tree levels   <<<<<<<<<<<<<<===================;;

;;----Returns a list of all emenents that are located on that same level of the tree
;;----Start counting levels from 1
(define (tree-level level tree)
  (cond
    ((empty? tree) '() )
    ((<= level 1) (list (root tree)) )
    ((append (tree-level (- level 1) (left tree))
             (tree-level (- level 1) (right tree))))))


;;---Returns a list of lists of all elemenets on every level of the tree
(define (tree-levels tree)
  (define height (height-tree tree) )
  (define (help level)
    (cond
      ((> level height) '() )
      ((append (list (tree-level level tree)) (help (+ level 1)) ))))

(help 1)
  )


;;================>>>>>>>>>>>>>>   Map a Tree   <<<<<<<<<<<<<<===================;;

(define (tree-map f tree)
  (cond
    [(empty? tree) '()]
    [(atom? (first tree)) (cons (f (first tree)) (tree-map f (rest tree)))]
    [else (cons (tree-map f (first tree)) (tree-map f (rest tree)))]
  )
)



;;$$$$$$$$$=========--------->  Binary Search Tree  <---------=========$$$$$$$$$;;

; Checks if x is an element of bsTree
(define (bst-element? x tree)
  (cond
    [(= x (root tree)) #t]
    [(is-leaf? tree) #f]
    [(< x (root tree)) (or #f (bst-element? x (left tree))) ]
    [else              (or #f (bst-element? x (right tree)))]))



; Traverse the tree in a such way that the list should contain sorted elements

;; Обхождане  ляво-корен-дясно връща числата в дървото, сортирани в НАРАСТВАЩ ред
(define (bst->list-increasing tree)   
  (if (empty? tree)
      '()
      (append (bst->list-increasing (left tree))
              (list (root tree))
              (bst->list-increasing (right tree)))))


;; Обхождане  ляво-корен-дясно връща числата в дървото, сортирани в НАМАЛЯВАЩ ред
(define (bst->list-decreasing tree)   
  (if (empty? tree)
      '()
      (append (bst->list-decreasing (right tree))
              (list (root tree))
              (bst->list-decreasing (left tree)))))



; Inserts x in the tree, returning a new BST with the proper structure
(define (bst-insert x tree)
  (cond
    [(empty? tree) (mkleaf x)]
    [(< x (root tree)) (mktree (root tree)
                               (bst-insert x (left tree))
                               (right tree))]
    [(> x (root tree)) (mktree (root tree)
                               (left tree)
                               (bst-insert x (right tree)))]
    [else tree]))




; Checks if the given binary tree is a binary search tree
(define (bst? tree)
  (apply < (bst->list-increasing tree)))


