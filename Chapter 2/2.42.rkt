#lang sicp


(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq)) (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))


(define (accumulate proc initial seq)
  (if (null? seq)
      initial
      (proc (car seq)
            (accumulate proc initial (cdr seq)))))


(define (append x y)
  (accumulate cons y x))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define empty-board (list))


(define (safe? k positions)
  (let ((row (caar positions))
        (col (cadar positions))
        (others (cdr positions)))
    (and (null? (filter (lambda (position) (= (car position) row)) others))
         (null? (filter (lambda (position) (= (abs (- row (car position))) (abs (- col (cadr position))))) others)))))
  

(define (adjoin-position row col queens)
  (cons (list row col) queens))
  

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

