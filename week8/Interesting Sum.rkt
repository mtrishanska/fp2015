#lang racket

(define (sum-div n)
  (define (help i res)
    (cond
      [(>= i n) res]
      [(= (remainder n i) 0)(help (+ i 1) (+ res i))]
      [else (help (+ i 1) res)]))
  (help 1 0))
         
(define (sum-interesting k)
  (cond
    [(= k 1) 0]
    [(= (sum-div (sum-div k)) k) (+ k (sum-interesting (- k 1)))]
    [else (sum-interesting (- k 1))]))
