#lang sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (sub-tree) (if (not (pair? sub-tree))
                                          1
                                          (count-leaves sub-tree)))
                   t)))

(define x (cons (list 1 2) (list 3 4)))

(count-leaves x)

(count-leaves (list x x))