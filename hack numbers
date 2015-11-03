#lang racket
(require "binary.rkt")
(define (palindrome? str)
  (equal? str (string-reverse str)))

(define (odd-num? str)
  (define (odd-helper? n res)
    (cond
      [(< n 0) ( = (remainder res 2) 1)]
      [(equal? (~a (string-ref str n) "1") (odd-helper? (- n 1) (+ res 1)))]
      [else (odd-helper? (- n 1) res)]))
    (odd-helper? (- (string-length str) 1) 0))

(define (hack-num n)
  (if (and (palindrome? (to-binary-string n) (odd-num? (to-binary-string n)))) #t
      #f))
(define (next-hack n)
  (if (hack-num (+ n 1)) (+ n 1)
                (next-hack (+ n 1))))
