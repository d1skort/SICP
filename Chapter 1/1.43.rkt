#lang sicp


(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f))


(define (repeated-iter f n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (compose f result))))
  (iter 1 f))


(define (square x) (* x x))


((repeated square 2) 5) ; 625
((repeated-iter square 2) 5) ; 625

((repeated inc 3) 5) ; 8
((repeated inc 3) 5) ; 8