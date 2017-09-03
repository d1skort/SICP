#lang sicp


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Недостаточно денег на счете"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass message)
    (if (eq? pass password)
        (cond ((eq? message 'withdraw) withdraw)
              ((eq? message 'deposit) deposit)
              (else "Неизвестный тип процедуры"))
        "Неверный пароль"))
  dispatch)