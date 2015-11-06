#lang racket
(define (string-reverse str)
  (define (helper n res)
   (if (<= n 0) res
       (helper (- n 1) (string-append res (~a (string-ref str (- n 1)))))))
  (helper (string-length str) ""))

(define (to-binary-string n)
  (string->number (number->string n 2)))

(define (from-binary-string binary-str)
  (number->string (string->number binary-str 2)))

