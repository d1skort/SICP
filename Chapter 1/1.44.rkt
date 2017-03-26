#lang sicp


(define dx 0.001)


(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated-iter f n)
  (define (iter result i)
    (if (= i n)
        result
        (iter (compose f result) (+ i 1))))
  (iter f 1))


(define (smooth f)
  (define (average a b c) ((+ a b c) / 3))
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))


(define (n-fold-smooth f n)
  ((repeated-iter smooth n) f))