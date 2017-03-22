#lang sicp


(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 1)
        (/ (n i) result)
        (iter (+ (d (- i 1))
                 (/ (n i) result))
              (- i 1))))
  (iter (d k) k))


(define (cont-frac-recursive n d k)
  (define (recursive i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (recursive (+ i 1))))))
  (recursive 1))