#lang sicp


(define (make-monitored f)
  (let ((called 0))
    (lambda (message)
      (cond ((eq? message 'how-many-calls?) called)
            ((eq? message 'reset-count) (set! called 0))
            (else (begin (set! called (inc called))
                         (f message)))))))