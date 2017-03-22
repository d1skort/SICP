#lang sicp


(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))


(define (sum-square-primes a b)
  (define (square x) (* x x))

  (filtered-accumulate + 0 square a inc b prime?))


(define (foo n)
  (define (filter i)
    (= (gcd i n) 1))
  
  (filtered-accumulate * 1 identity 1 inc n filter))