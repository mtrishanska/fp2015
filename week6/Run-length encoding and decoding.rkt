#lang racket
(define (run-length-encode str)
  (let ([fst (substring str 0 1)])
       ([rst (substring str 1)])
  (define (help xs res count)
    (cond
      [(empty? xs) ""]
      [(equal? fst (fst(rst))) (help rest res (+ 1 count))]
      [(= count 1) (help (rst xs) 1 (string-append (fst xs) res))]
      [else (help (rst xs) 1 (string-append res (~a count) (fst xs)))]))
    (help str "" 1)))

