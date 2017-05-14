#lang sicp


; part a
(define (make-mobile left right)
  (list left right))


(define (make-branch length structure)
  (list length structure))


(define (left-branch mobile)
  (car mobile))


(define (right-branch mobile)
  (car (cdr mobile)))


(define (branch-length branch)
  (car branch))


(define (branch-structure branch)
  (car (cdr branch)))


; part b
(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight branch)
      (branch-structure branch)))


(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))


; part c
(define (branch-moment branch)
  (* (branch-length branch) (branch-weight branch)))


(define (balanced? mobile)
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (balanced? (branch-structure branch))
        #t))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (branch-moment left) (branch-moment right)))))