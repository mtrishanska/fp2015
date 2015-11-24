#lang racket
(define (tree-level n tree)
   (cond
    [(empty-tree? tree) '()]
    [(= n 1) (list(node tree))]
    [else (append (tree-level (- n 1) (left tree))
                  (tree-level (- n 1) (right tree)))]))

(define (tree-levels tree)
  (define (help n xs res)
    (cond
      [(= n 0) res]
      [else (help (- n 1) xs (cons (tree-level n xs) res))]))
  (help (height tree) tree '()))
