#lang sicp


(define (make-accumulator start)
  (lambda (value)
    (begin (set! start (+ value start))
           start)))