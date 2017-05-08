#lang sicp


(define (filter pred seq)
  (cond ((null? seq) seq)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))


(define (same-parity x . y)
  (if (even? x)
      (cons x (filter even? y))
      (cons x (filter odd? y))))
