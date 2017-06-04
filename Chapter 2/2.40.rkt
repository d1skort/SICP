#lang sicp


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


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))