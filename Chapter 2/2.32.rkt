#lang sicp


(define (append x y)
  (cond ((null? x) y)
        ((null? y) x)
        (else (cons (car x) (append (cdr x) y)))))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append x (list (car s)))) rest)))))