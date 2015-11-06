#lang racket
 (define (string-repeat str n)
   (cond
     [(= n 1) str]
     [else (string-append str (string-repeat str (- n 1)))]))

 (define (fence n)
   (string-append "{" (string-repeat "-" (round (+ 1 (log n)))) ">" (number->string n) "<" (string-repeat "-" (round (+ 1 (log n)))) "}"))
