#lang sicp


(define (install-sum-product-package)
  (define (make-sum a1 a2)
    (list '+ a1 a2))

  (define (addend s) (car s))

  (define (augend s) (cadr s))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend s) var)
              (deriv (augend s) var)))

  (define (make-product p1 p2)
    (list '* p1 p2))

  (define (multiplier p) (car p))

  (define (multiplicand p) (cadr p))

  (define (deriv-product exp var)
    (make-sum
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)