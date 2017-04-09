#lang sicp


(define (cons a b)
  (* (power 2 a) (power 3 b)))

(define (car pair)
  (factor 2 pair))

(define (cdr pair)
  (factor 3 pair))

(define (factor base value)
  (define (factor-iter value counter)
    (if (= (remainder value base) 0)
        (factor-iter (/ value base) (+ counter 1))
        counter))
  (factor-iter value 0))

(define (power base exponent)
  (define (power-iter product counter)
    (if (= counter 0)
        product
        (power-iter (* product base) (- counter 1))))
  (power-iter 1 exponent))