#lang sicp


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (car set) x) (cons (car set) (adjoin-set x (cdr set))))
        ((> (car set) x) (cons x set))
        (else set)))