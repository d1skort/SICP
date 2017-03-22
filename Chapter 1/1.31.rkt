#lang sicp


(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))


(define (factorial n)
  (product identity 1 inc n))


(define (pi-product a b)
  (define (pi-term x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  
  (* 4.0 (product-recursive pi-term a inc b)))