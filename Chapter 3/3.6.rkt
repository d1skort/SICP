#lang sicp


(define rand
  (let ((x rand-init))
    (define (dispatch message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-value) (set! x new-value)))))
    dispatch))

; (rand 'generate)
; ((rand 'reset) new-value)