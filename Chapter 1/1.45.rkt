#lang sicp


(define tolerance 0.00001)


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (average a b)
  (/ (+ a b) 2))


(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (define (iter result i)
    (if (= i n)
        result
        (iter (compose f result) (+ i 1))))
  (iter f 1))


(define (power a b)
  (exp (* b (log a))))


(define (log2 x)
  (/ (log x) (log 2)))
  

(define (nth-root n x)
  (define (f y) (/ x (power y (- n 1))))
  (let ((damp-count (floor (log2 n))))
    (fixed-point ((repeated average-damp damp-count) f) 1.0)))