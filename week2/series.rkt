#lang racket
(define (series a b n)
  (define (fib-iter a b i)
    (if (> i n) b
      (fib-iter b (+ a b) (+ i 1))))
  (fib-iter a b 3))

(define (lucas n)
  (define (series a b i)
    (if (> i n) b
        (series b (+ a b) (+ i 1))))
  (series 2 1 0))

(define (fibonacci n)
  (define (series a b i)
    (if (> i n) b
        (series b (+ a b) (+ i 1))))
  (series 1 1 0))

(define (summed-member n)
  (+ (lucas n) (fibonacci n)))
    
(define (nth-lucas-sums n)
  (- (lucas (+ n 2)) 1))

(define (nth-fibonacci-sums n)
  (- (fibonacci (+ n 2)) 1))

(define (lucas-fib-dif n)
  (- (lucas n) (fibonacci n)))
