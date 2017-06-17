#lang sicp


(define (accumulate proc initial seq)
  (if (null? seq)
      initial
      (proc (car seq)
            (accumulate proc initial (cdr seq)))))


(define (filter proc seq)
  (cond ((null? seq) nil)
        ((proc (car seq)) (cons (car seq) (filter proc (cdr seq))))
        (else (filter proc (cdr seq)))))


(define (variable? x) (symbol? x))


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product p1 p2)
  (cond ((or (=number? p1 0) (=number? p2 0)) 0)
        ((=number? p1 1) p2)
        ((=number? p2 1) p1)
        ((and (number? p1) (number? p2)) (* p1 p2))
        (else (list '* p1 p2))))


(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


(define (addend s) (cadr s))


(define (augend s)
  (let ((others (cddr s)))
    (if (= (length others) 1)
        (car others)
        (cons '+ others))))


(define (product? x)
  (and (pair? x) (eq? (car x) '*)))


(define (multiplier p) (cadr p))


(define (multiplicand p)
  (let ((others (cddr p)))
    (if (= (length others) 1)
        (car others)
        (cons '* others))))


(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))


(define (exponent e) (caddr e))


(define (base e) (cadr e))


(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else #f)))