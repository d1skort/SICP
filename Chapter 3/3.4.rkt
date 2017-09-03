#lang sicp


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount) (begin (set! balance (- balance amount)) balance)
        "Insufficient funds")) 

  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else "Unknown request -- MAKE-ACCOUNT")))

  (define (protect-with-password p f)
    (let ((invalid-tries 0))
      (lambda (pass arg)
        (if (eq? pass p)
            (begin (set! invalid-tries 0)
                   (f arg))
            (begin (set! invalid-tries (inc invalid-tries))
                   (if (= invalid-tries 7)
                       (lambda (x) "Too many wrong attempts. Cops have been called")
                       (lambda (x) "Incorrect password")))))))

  (protect-with-password password dispatch))