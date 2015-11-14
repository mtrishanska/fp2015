#lang racket
(define (sum numbers)
  (cond
    [(empty? numbers) 0]
    [else (+ (first numbers) (sum (rest numbers)))]))

(define (member? x items)
  (cond
    [(empty? items) #f]
    [(equal? (first items) x) #t]
    [else (member? x (rest items))]))

(define (length2 items)
  (cond
    [(empty? items) 0]
    [else (+ 1 (length2 (rest items)))]))

(define (list-ref2 items n)
  (cond
    [(= n 0) (first items)]
    [else (list-ref2  (rest items) (- n 1))]))

(define (range2 a b)
  (define (helper i num)
     (cond
         [(> a i) num]
         [else  (helper (- i 1) (cons i num ))]))
  (helper  (- b 1) '()))

(define (build-list2 n f)
  (define (helper i res)
    (cond
      [(< i 0 ) res]
      [else (helper (- i 1) (cons (f i) res))]))
  (helper (- n 1) '()))

(define (append2 l1 l2)
  (cond
    [(empty? l1) l2]
    [else (cons (first l1) (append2 (rest l1) l2))]))

(define (reverse2 items)
  (define (helper res it)
  (cond
    [(empty? it) res]
    [else (helper (cons (first it) res) (rest it))]))
  (helper '() items))

(define (take2 n items)
 (define (help i xs)
  (cond
    [(empty? xs) (list)]
    [(= n 0) (list)]
    [else (cons (first xs) (take2 (+ i 1) (rest xs)))]))
  (help 0 items))

(define (drop2 n items)
  (cond
    [(empty? items) (list)]
    [(= n 0) items]
    [else  (drop2 (- n 1) (rest items))]))

(define (take-while p items)
  (define (helper xs res)
          (cond
            [(not (p (first xs)))(reverse res)]
            [else (helper (rest xs)(cons (first xs) res))]))
  (helper items(list)))

(define (drop-while p items)
  (define (helper xs i)
    (cond
      [(not (p (first xs))) (drop2 i items)]
      [else (helper (rest xs) (+ i 1))]))
  (helper items 0))

(define (number->list n)
  (define (helper n xs)
    (cond
      [(= n 0) xs]
      [else (helper (quotient n 10) (cons (remainder n 10) xs))]))
  (helper n (list)))

(define (list->number xs)
  (define (helper lst res)
    (cond
      [(empty? lst) res]
      [else (helper (rest lst) (+ (* res 10) (first lst)))]))
  (helper xs 0))
  

