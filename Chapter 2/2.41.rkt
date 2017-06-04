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


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) (cons i pair))
                  (unique-pairs (- i 1))))
           (enumerate-interval 3 n)))


(define (triples-with-sum n s)
  (filter (lambda (t) (= (accumulate + 0 t) s)) (unique-triples n)))
