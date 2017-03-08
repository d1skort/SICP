#lang sicp


(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 1)
        (/ (n i) result)
        (iter (+ (d (- i 1))
                 (/ (n i) result))
              (- i 1))))
  (iter (d k) k))


(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))