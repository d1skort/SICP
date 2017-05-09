#lang sicp


(define (reverse lst)
  (define (iter lst result)
    (if (null? lst)
        result
        (iter (cdr lst) (cons (car lst) result))))
  (iter lst nil))


(define (deep-reverse lst)
  (define (iter lst result)
    (cond ((null? lst) result)
          ((pair? (car lst)) (iter (cdr lst) (cons (deep-reverse (car lst)) result)))
          (else (iter (cdr lst) (cons (car lst) result)))))
  (iter lst nil))


(define x (list (list 1 2) (list 3 4)))

(display (reverse x))
(newline)
(display (deep-reverse x))