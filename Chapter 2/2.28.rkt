#lang sicp


(define (append x y)
  (cond ((null? x) y)
        ((null? y) x)
        (else (cons (car x) (append (cdr x) y)))))


(define (fringe lst)
  (cond ((null? lst) lst)
        ((not (pair? (car lst))) (cons (car lst) (fringe (cdr lst))))
        (else (append (fringe (car lst)) (fringe (cdr lst))))))


(define x (list (list 1 2) (list 3 4)))

(display (fringe x))
(newline)
(display (fringe (list x x)))