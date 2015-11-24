#lang racket
(define (bst-insert x tree)
     (cond
       [(empty-tree? tree) x]
       [(= x (node tree)) (make-tree (bst-insert x (node tree))
                                         (left tree)
                                         (right tree))]
       [(< x (node tree)) (make-tree (node tree)
                                     (bst-insert x (left tree))
                                     (right tree))]
       [else (make-tree (node tree)
                        (left tree)
                        (bst-insert x (right tree)))]))

(define (bst-element? x tree)
  (cond
    [(empty-tree? tree) #f]
    [(= x (node tree)) (node tree)]
    [(< x (node tree)) (bst-element? x (left tree))]
    [else (bst-element? x (right tree))]))

(define (bst->list tree)
  (cond
    [(empty-tree? tree) '()]
    [else (append (bst->list (left tree))(list (node tree)) (bst->list (right tree)))]))

(define (bst? tree)
  (define (help xs)
    (cond
      [(< (first xs) (second xs)) #t]
      [(> (first xs) (second xs)) #f]
      [else (help (rest xs))]))
  (help (bst->list tree)))
