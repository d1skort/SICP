#lang sicp


(define (for-each f seq)
  (cond ((null? seq) #t)
        (else
         (f (car seq))
         (for-each f (cdr seq)))))


(for-each (lambda (x) (newline) (display x)) (list 57 321 88))