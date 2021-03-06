#lang racket
(define (group xs)
  (define (help res lst)
    (let ([x (lambda (x) (equal? x(first lst)))])
    (cond
      [(empty? lst) res]
      [else (help (cons (take-while x lst) res)
                  (drop-while x lst))])))
 (reverse (help (list) xs)))
