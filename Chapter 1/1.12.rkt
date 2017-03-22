#lang racket

; zero-based
(define (triangle-elem i j)
  (if (or (= j 0) (= j i))
      1
      (+ (triangle-elem (- i 1) (- j 1))
         (triangle-elem (- i 1) j))))