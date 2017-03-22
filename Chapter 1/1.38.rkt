#lang sicp


(define (cont-frac n d k)
  (define (iter result i)
    (if (= i 1)
        (/ (n i) result)
        (iter (+ (d (- i 1))
                 (/ (n i) result))
              (- i 1))))
  (iter (d k) k))


(define (e k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i) (if (= (remainder i 3) 2)
                                  (* 2 (/ (+ i 1) 3))
                                  1))
                  k)))