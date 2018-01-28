#lang sicp


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Недостаточно средств"))

  (define (deposit amount)
    (set! balance (+ balance amount)))

  (define (make-joint new-password)
    (protected-with-password new-password dispatch))
  
  (define (dispatch message)
    (cond ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit) deposit)
          ((eq? message 'make-joint) make-joint)
          (else (lambda (arg1 . args) "Неизвестная операция"))))

  (define (protected-with-password original-password dispatch)
    (lambda (provided-password message)
      (if (eq? provided-password original-password)
          (dispatch message)
          "Неверный пароль!")))

  (protected-with-password password dispatch))


(define (make-joint account old-password new-password)
  ((account old-password 'make-joint) new-password))
